//===================================================================
//   Проект КОРВЕТ-Extrom, микропрограмма внешнего контроллера
// Предназначен для эмуляции дисков CP/M через боковой разъем ППИ 3
//===================================================================
//  Автор - forth32 (Alexader Stepanov),  forth32@mail.ru
//
//  Процессор - Atmega32, 8 MHz, внутренний Rc-генератор
//
//  Подключение периферии к портам процессора:
//
// Pin  сигнал AVR   Описание
//=======================================================================
//  1       PB0    Светодиод индикации обращения к диску (и просто для отладки), анодом к +5V, катодом сюда через резистор
//  4       PB3    Кнопка сброса конфигурации
//  5       -SS    Сигнал выбора SPI-устройства, подключается ко входу SS карты
//  6       MOSI   Выход данных SPI, ко входу данных карты (DI)
//  7       MISO   Вход данных SPI, к выходу данных карты (DO)
//  8       MSCK   Сигнал тактирования SPI, ко входу синхронизации карты (SC)
//  9       RESET  Кнопка сброса, обязательно подтянуть к +5v!
// 14       RxD    Приемник последовательного порта
// 15       ТxD    Передатчик последовательного порта
// 16       Int0   Вход сигнала control для определения презапуска корвета
// 17       Int1   Вход сигнала -OBF (PC7 BB55) - запрос  от корвета на передачу байта
// 18       PD4    Выход сигнала -ACK (PC6 BB55) - подтверждение приема байта от корвета, длина не менее 300 нс
// 19       PD5    Вход сигнала IBF (PC5 ВВ55) - подтверждение передачи байта в корвет
// 20       PD6    Выход сигнала -STB (PC4 BB55) - запрос на передачу байта в корвет
// 21       PD7    Адрес A0 эмулируемого ПЗУ, подключается у порту PB0 ВВ55
// 33-40 PA0..7    Порт ввода данных PA0-PA7 ВВ55
//
//

#define VERSION "1.3"

#define F_CPU 8000000UL  // 8 MHz

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <util/delay.h>
#include <avr/pgmspace.h>

#include "fs.h"

// Параметры кольцевых буферов накопления UART
#define USART_RX_BUFFER_SIZE 16     // размер буфера приемника
#define USART_TX_BUFFER_SIZE 128     // размер буфера передатчика
#define USART_RX_BUFFER_MASK ( USART_RX_BUFFER_SIZE - 1 )  // битовая маска маркеров приемника
#define USART_TX_BUFFER_MASK ( USART_TX_BUFFER_SIZE - 1 )  // битовая маска маркеров передатчика

// Распределение кольцевых буферов в памяти
static unsigned char USART_RxBuf[USART_RX_BUFFER_SIZE]; // буфер приемника
static volatile unsigned char USART_RxHead;				// маркер начала
static volatile unsigned char USART_RxTail;				// маркер конца
static unsigned char USART_TxBuf[USART_TX_BUFFER_SIZE]; // буфер передатчика
static volatile unsigned char USART_TxHead;				// маркер начала
static volatile unsigned char USART_TxTail;				// маркер конца

static unsigned char fb[128];     // буфер для страницы загрузчика, а также логических блоков CP/M

// процедуры обслуживания консольного псевдофайлового потокоа вывода
static int fputchar(char c, FILE *stream);
static FILE mystdout = FDEV_SETUP_STREAM(fputchar, 0, _FDEV_SETUP_WRITE);

unsigned char kbhit( void );
void readstr();

static volatile unsigned char t1cnt;    // счетчик переполнений таймера 1
static volatile unsigned char led_flag; // 0 - индикация запрещена, 1 - разрешена

static volatile unsigned char boot_flag;   // Флаг начальной загрузки (фаза 1): 0 - корвет еще не загружен, 1 - процесс начальной загрузки окончен

static unsigned char diskfolder[14]; // имя каталога для вновь монтируемых образов
static volatile char lspt[5];   // Число логических секторов на дорожку для каждого из образов
static volatile char systracks;   // Число системных дорожек для образа диска А
static unsigned char Fname[5][14]; // имя файла образа
static unsigned char Ffolder[5][14]; // имя каталога, хранящего файл образа
static unsigned char bios_flag=0;  // флаг действующей подмены образа A на образ BIOS
static unsigned char enable_bios_substitute_flag=1;  // флаг разрешения такой подмены
static unsigned char roflag[5];		  // флаг "только чтение" каждого диска: 0-чтение/запись  1-только чтение
static unsigned char fstatus[5];          // флаг результата монтирования: 0 - не смонтирован, 1 - смонтирован

// Образ 32 начальных байтов инфосектора, записываемых в создаваемый образ диска
const char PROGMEM infosector[]= {0x80, 0xc3, 0x00, 0xda, 0x0a, 0x00, 0x00, 0x01, 0x01, 0x01, 0x03, 0x01, 0x05, 0x00, 0x50, 0x00,
                                  0x28, 0x00, 0x04, 0x0f, 0x00, 0x8a, 0x01, 0x7f, 0x00, 0xc0, 0x00, 0x20, 0x00, 0x02, 0x00, 0x10
                                 };

// Имена файлов, подставляемых для загрузки системных дорожек
char* sysfiles[3]= {"SYSTEM.BIN","MICRODOS.BIN","SYSTEMn.BIN"};
char substitute_number=0;

//------------------- Процедуры обслуживания последовательного порта ---------------------------

//***********************************************
//* Обработчик прерывания от приемника USART    *
//***********************************************
ISR (USART_RXC_vect) {

    unsigned char data;
    unsigned char i;

    data = UDR; // принятый байт данных
    // вычисление маркера начала буфера
    i = ( USART_RxHead + 1 ) & USART_RX_BUFFER_MASK;
    USART_RxHead = i;
    USART_RxBuf[i] = data; // сохраняем байт байт в буфере
}

//*************************************************
//* Обработчик прерывания от передатчика USART    *
//*************************************************
ISR (USART_UDRE_vect) {

    unsigned char i;

    if ( USART_TxHead != USART_TxTail ) {
    // если еще не все данные переданы
        i = (USART_TxTail + 1) & USART_TX_BUFFER_MASK; // вычисляем маркер конца буфера
        USART_TxTail = i;
        UDR = USART_TxBuf[i];  // выводим очередной байт из буфера в передатчик
    }
    else  {
    // все данные уже переданы
        UCSRB &= ~(1<<UDRIE); // запрещаем прерывания от передатчика
    }
}

//**************************************************
//* Ожидание полного освобождения буфера USART     *
//**************************************************
void pflush() {
    while (USART_TxHead != USART_TxTail);
}

//******************************************
//* Передача байта для посылки в USART     *
//******************************************

void putch(char data) {

    unsigned char i;


    i = ( USART_TxHead + 1 ) & USART_TX_BUFFER_MASK; // вычисляем новый маркер начала
    while ( i == USART_TxTail );  // Ждем освобождения места в буфере
    USART_TxBuf[i] = data;  // Сохраняем данные в буфере
    USART_TxHead = i;       // новый индекс начала
    UCSRB |= (1<<UDRIE);          // открываем прерывания от передатчика
}

//******************************************
//* Получение очередного принятого байта   *
//******************************************

char getch(void) {

    unsigned char i;

    while ( USART_RxHead == USART_RxTail );  // Ждем появления байта в буфере
    i = ( USART_RxTail + 1 ) & USART_RX_BUFFER_MASK; // вычисляем новый маркер конца
    USART_RxTail = i;
    putch(USART_RxBuf[i]);   // эхо-печать введенного байта
    return USART_RxBuf[i];   // возвращаем байт из буфера
}

//**********************************
//*  Печать байта в HEX            *
//**********************************
void printhex(unsigned char c) {

    unsigned char r;

    r=((c>>4)&0xf)+'0';
    if (r>'9') r+=7;
    putch(r);		// старший байт

    r=(c&0xf)+'0';
    if (r >'9') r+=7;
    putch(r);		// младший байт
}

//**********************************
//* Вывод байта в файловый поток   *
//**********************************
static int fputchar(char c, FILE *stream) {

    if (c == '\n') fputchar('\r', stream); // заменяем <cr> на <cr><lf>
    putch(c);
    return 0;
}

//***************************************
//* Печать строки фиксированной длины   *
//***************************************
void printstrl(unsigned char* str,unsigned  char len) {

    char i;
    for(i=0; i<len; i++) fputchar(str[i],stdout);
}

//****************************************************************
//* Проверка наличия непрочитанных байт в буфере приемника USART *
//****************************************************************

unsigned char kbhit( void )  {
    return ( USART_RxHead != USART_RxTail );
}


//------- Процедуры работы с таймером и индикация ошибок ---------------------

//****************************************************
//*  Прерывание по перезагрузке таймера 1 из OC1A    *
//****************************************************

ISR (TIMER1_COMPA_vect) {

    t1cnt++;
    if (led_flag == 1)  PORTB^=_BV(0);           // переключаем светодиод
}

//****************************************************
//*  Прерывание по перезагрузке таймера 1 из OC1A    *
//*                                                  *
//*   Бесконечно вымаргивает светодиодом код ошибки  *
//****************************************************

void led_error(unsigned char code) {

    // настройка таймра 1
    GICR&=~_BV(INT1);                       // Запрещаем внешнее прерывание от строба данных интерфейса
    TCCR1B=_BV(WGM12)|_BV(CS12)|_BV(CS10);  // CTC, частота - clk/1024
    OCR1A=2500;                             // делитель таймера
    DDRB|=_BV(0);
    PORTB|=_BV(0);                          // отключаем светодиод
    TCNT1=0;				// перезапускаем таймер
    TIMSK|=_BV(OCIE1A);			// разрешаем прерывания от таймера
    // бесконечный цикл индикации
    for(;;) {
        t1cnt=0;
        led_flag=1;                 // разрешаем индикацию
        while (t1cnt < (2*code));     // цикл индикации - вымаргивает код
        t1cnt=0;
        led_flag=0;                 // запрещаем индикацию
        while (t1cnt < 6);          // цикл паузы индикации
    }
}


//--------  Процедуры работы с SPI и SD-картой ---------------


//*************************************
//* Отправка и прием байта через SPI  *
//*************************************
unsigned char spi_send(unsigned char c) {

    SPDR = c;        // отправляем байт
    while((SPSR & (1<<SPIF)) == 0);  // ждем ответа
    return SPDR;   // выходной байт
}

#define spi_receive() spi_send(0xff)    // макрос приема байта из SPI

//*************************************
//* Отправка команды SD-карте         *
//*************************************
unsigned char send_sd_cmd(char cmd, unsigned long adr) {

    unsigned char res;
    unsigned int pass;

    spi_send (cmd|0x40);	// код команды

    spi_send ((adr>>24)&0xff);	// адрес
    spi_send ((adr>>16)&0xff);
    spi_send ((adr>>8)&0xff);
    spi_send (adr&0xff);

    spi_send (0x95);   // CRC

    // Ждем ответа R1
    for(pass=0; pass<0xffff; pass++) {
        res=spi_receive();
        if ((res&0x80)==0) break;
    }
    if (pass != 0xffff)return res;
    else return 0xaa;
}

//**************************************************
//*   Чтение ответа на команду SD-карте            *
//**************************************************
unsigned char read_sd_data(unsigned char* buf,unsigned int len,unsigned int offset) {

    unsigned char res;
    unsigned int i;
    unsigned int ln=0;

    for(i=0; i<0xffff; i++)  {
        //Ждем начала пакета данных
        res=spi_receive();   // должен быть маркер FE
        if (res==0xfe) break;
    }

    if (i==0xffff) {
        printf_P(PSTR("\n Таймаут данных - %02x  %i"),res,len);
        return 1;
    }

    // Цикл побайтового приема всего сектора
    for (i=0; i<512; i++) {
        res=spi_receive();
        if ((i<offset)||(i>offset+len)) continue;  // вырезаем из всего потока байтов нужный кусок
        buf[ln++]=res;
    }
    spi_receive();	// CRC-H
    spi_receive();  // CRC-L
    return 0;
}

//**************************************************
//*   Чтение блока с SD-карты                      *
//**************************************************

unsigned char read_block (unsigned char* buf, unsigned long adr, unsigned int len,unsigned int offset) {

    unsigned char res;

    //printf_P(PSTR("\n - read: %lx - %i"),adr,len);
    res=send_sd_cmd(17,adr);	// CMD17 - чтение одного блока

    if (res!=0x00) return res;	//ошибка
    spi_send (0xff);
    return read_sd_data(buf,len,offset);
}

//**************************************************
//*   Запись блока на SD-карту                     *
//**************************************************
unsigned char write_block (unsigned char* buf, unsigned long adr) {

    unsigned char res;
    unsigned int i;

    res=send_sd_cmd(24,adr);	// CMD24 - запись одного блока
    if (res!=00) return res;	// ошибка
    spi_send (0xff);

    spi_send (0xfe);	// Токен начала данных
    for (i=0; i<512; i++)  spi_send(buf[i]); // Блок данных

    spi_send (0xff);	// CRC
    spi_send (0xff);

    res=spi_receive();
    if ((res&0x05)==0) return 0xaa;	// Ошибка - блок данных отвергнут картой

    i=0;
    for(i=0; i<0xffff; i++) { //Ждем окончания busy состояния
        res=spi_receive();
        if (res==0xff) break;
    }
    if (i==0xffff) return 0xaa;  // ошибка - таймаут записи
    return 0;
}



//**************************************
//* Чтение байта из EEPROM
//**************************************
unsigned char EEread(int adr) {

    EEAR=adr;
    EECR|=(1<<EERE);   // строб чтения
    return EEDR;  // выбираем байт и возвращаем его
}

//========================================================================================================================================

//**************************************
//* Int0 - от сигнала control          *
//**************************************

//   CONTROL - сигнал, вырабатываемый ОПТС
//  Положительный фронт - начало последовательности загрузки
//  Отрицательный фронт - запрос на отключение от шины, если не отработать - будет ошибка шины ОПТС
//        также отрицательный фронт возникает при перезагрузке, практически - в момент сброса ВВ55
//
ISR (INT0_vect) {

    unsigned int adr;
    unsigned char passcount;

    if (bit_is_clear(PIND,2)) {
        if (boot_flag == 0) {
            // Сигнал Control ушел в 0, а корвет отрабатывает ОПТС - немедленно отключаемся от шины !
            DDRA=0;   // порт - на ввод
            PORTA=0; // отключаем все подтяжки
            boot_flag=0; // сбрасываем флаг загрузки
            DDRD=_BV(1);     // TxD на вывод, остальные линиии порта на ввод во избежании конфликтов с адресом на канале C
            return;
        }
        else {
            // Сигнал Control ушел в 0, а корвет уже загружен
            _delay_ms(1);  // ждем 1 ms
            if (bit_is_set(PIND,2)) return; // сигнал опять ушел к 1 - это была помеха, игнорируем ее
            asm volatile(" jmp 0");  // уходим в перезагрузку
        }
    }

    // Сигнал control стал 1 - начинаем процесс начальной загрузки
    DDRA=0xff;   // шину - на вывода
    //sei();       // разрешаем новое прерывание для перезапуска системы
    // Фиксированная последовательность адресов 4 5 6 7
    PORTA=EEread(4);
    while(bit_is_clear(PIND,7));
    PORTA=EEread(5);
    while(bit_is_set(PIND,7));
    PORTA=EEread(6);
    while(bit_is_clear(PIND,7));
    PORTA=EEread(7);
    while(bit_is_set(PIND,7));
    // Далее выдаем по кольцу данные с адресов 00-FF
    adr=0;
    passcount=0;  // счетчик полных проходов
    // цикл полной передачи данных за 3 прохода
    while(passcount<3) {
        PORTA=EEread(adr++);	          // четные байты
        while(bit_is_clear(PIND,7));
        PORTA=EEread(adr++);            // нечетные байты
        while(bit_is_set(PIND,7));
        if (adr >= 0x100) {
            adr=0;         // заворачиваем адрес
            passcount++;   // счетчик проходов++
        }
    }
    // 3 прохода отработали - настраивает порты в основной режим работы

    DDRD=0x52;      // рабочий режим - TxD,-STB, -ACK - на вывод
    PORTD=0x50;	    // -STB=1    -ACK=1
    boot_flag=1;    // взводим флаг загрузки
}


//*************************************************
//* HEX-дамп области памяти                       *
//*
//*  offset добавляется к адресу при печати
//*************************************************

void dump(char* sbuf, int len) {
    int i,j;
    unsigned char ch;

    fputchar('\n',stdout);
    for (i=0; i<len; i+=16) {
        printhex(i>>8);
        printhex(i&0xff);
        printstrl(": ",2);
        for (j=0; j<16; j++) {
            if ((i+j) < len) {
                printhex(sbuf[i+j]&0xff);
                putch(' ');
            }
            else printf_P(PSTR("   "));
        }
        printf_P(PSTR(" *"));
        for (j=0; j<16; j++) {
            if ((i+j) < len) {
                ch=sbuf[i+j];
                if ((ch < 0x20)||(ch > 0x7e)) putch('.');
                else putch(ch);
            }
            // заполнение пробелами для неполных строк
            else putch(' ');
        }
        printf_P(PSTR("*\n"));
    }
}


//*============ Процедуры интерфейса с портом корвета КР580ВВ55  IOP3 ===========================

//******************************************************************
//*  Пересылка байта в корвет через двунаправленный порт А ВВ55
//******************************************************************
void iop_send_byte(unsigned char c) {

    while(bit_is_set(PIND,5));   // ждем IBF=0 - освобождения буфера ввода
    PORTA=c;                     // выводимый байт
    PORTD &= 0xbf;               //pd6 (-STB) = 0
    while(bit_is_clear(PIND,5));   // ждем IBF=1 - подтверждение приема
    PORTD |= 0x40;               //pd6 (-STB) = 1 - снимаем строб
}

//*************************************
//*     Отправка буфера в корвет
//*************************************
void iop_sendbuf(unsigned char* buf, unsigned int size) {

    unsigned int i;

    for(i=0; i<size; i++) {
        while(bit_is_set(PIND,5));   // ждем IBF=0 - освобождения буфера ввода
        PORTA=buf[i];                // выводимый байт
        PORTD &= 0xbf;               //pd6 (-STB) = 0
        while(bit_is_clear(PIND,5));   // ждем IBF=1 - подтверждение приема
        PORTD |= 0x40;               //pd6 (-STB) = 1 - снимаем строб
    }
}

//******************************************************************
//*  Прием байта из корвета через двунаправленный порт А ВВ55
//******************************************************************
unsigned char iop_get_byte() {

    unsigned char c;
    while(bit_is_set(PIND,3)); // ждем OBF=0 - готовность данных в буфере вывода
    DDRA=0;		           // порт на ввод!
    PORTD &= 0xef;             //pd4 (-ACK) = 0
    asm volatile("nop");       // обязательная задержка на обработку сигнала портом!!!
    asm volatile("nop");
    asm volatile("nop");
    asm volatile("nop");
    c=PINA;                  // забираем вводимый байт
    PORTD |= 0x10;  	 //pd4 (-ACK) = 1 - снимаем строб
    while(bit_is_clear(PIND,3));   // ждем OBF=1 - снятие готовности данных
    DDRA=0xFF;		 // порт на вывод
    return c;
}

//*************************************
//*     Прием буфера из корвета
//*************************************
void iop_getbuf(unsigned char* buf, unsigned int size) {

    unsigned char c;
    unsigned int i;
    DDRA=0;		           // порт на ввод!

    for(i=0; i<size; i++) {
        while(bit_is_set(PIND,3)); // ждем OBF=0 - готовность данных в буфере вывода
        PORTD &= 0xef;             //pd4 (-ACK) = 0
        asm volatile("nop");       // обязательная задержка на обработку сигнала портом!!!
        asm volatile("nop");
        asm volatile("nop");
        asm volatile("nop");
        c=PINA;                  // забираем вводимый байт
        PORTD |= 0x10;  	  //pd4 (-ACK) = 1 - снимаем строб
        buf[i]=c;		  // сохраняем полученный байт
        while(bit_is_clear(PIND,3));   // ждем OBF=1 - снятие готовности данных
    }
    GIFR|=_BV(INTF1);	 // сбрасываем ложное ждущее прерывание от обмена данными
    DDRA=0xFF;		 // порт на вывод
}


//***********************************************************
//*   Заливка в корвет загрузчика 2 фазы
//*
//*  Берется из файла STAGE2.ROM или одного из ROMn.BIN
//***********************************************************
void send_loader() {

    unsigned char loader_size;  // размер загрузчика в блоках по 256 байт
    unsigned int res,blk,sts;
    unsigned char* sbuf=getsbufptr();
    unsigned char nfile;  // # файла для загрузки


    printf_P(PSTR("\nЗагрузка Stage2\n"));

    // Принимаем от загрузчика номер файла
    nfile=iop_get_byte();
    // Формируем имя файла
    //ESL    if (nfile == 8) strcpy(sbuf, "LOADER.BIN");
    if (nfile == 8) strcpy(sbuf, "STAGE2.ROM");
    else {
        strcpy(sbuf,"ROMX.BIN");
        sbuf[3]='0'+nfile;
    }
    printf_P(PSTR("\nLoader file: %s"), sbuf);

    res=fs_open();
    if (res != 0) {
        printf_P(PSTR("\n - нет файла!"));
        led_error(2);
    }
    fs_getfilesize();
    loader_size=fs_tmp>>8;   // вычисляем размер файла в 256-байтовых блоках

    fs_lseek(6,0);    // +6 - старший байт адреса загрузки
    fs_read(fb, 1, &res);


    iop_send_byte(fb[0]);         // отсылаем загрузчику адрес размещения файла в памяти
    printf_P(PSTR("\nLoader base: %x"), fb[0]);
    iop_send_byte(loader_size);   // и число загружаемых блоков
    printf_P(PSTR("\nLoader size: %x"), loader_size);

    // Пересылка загрузчику всех блоков файла
    fputchar('\n',stdout);
    fs_lseek(0,0);    // встаем на начало файла
    for(blk=0; blk<loader_size*2; blk++) {
        sts=fs_read(fb, 128, &res);
        if (sts != 0) {
            printf_P(PSTR("\n ошибка чтения %i"),lastError);
            break;
        }
        if (res == 0) break;     // конец файла
        iop_sendbuf(fb,128);     // отсылаем очередные 256 байт
        printf_P(PSTR("\r Block # %i"),blk);
    }
    printf_P(PSTR("\nЗагрузка Stage2 окончена"));
}

//######################################### Далее идут процедуры работы с образами дисков и программным API ############################

//**********************************************************************************************
//*   Монтирование образа KDI
//*   С образа считывается и разбирается информационный сектор для получения SPT и других парамтеров
//*
//*  dsk - номер диска, 0 или 1
//*  filename - имя файла, не более 8.3, заканчивается 0.
//*  prefix=0 - монтирование из корневого каталога
//*         1 - из каталога DISK
//**********************************************************************************************
void mount_disk(char dsk,char* filename, char prefix) {

    unsigned char* sbuf=getsbufptr();  // буфер сектора VinxFS
    unsigned int res;

    if (dsk>4) {
        printf_P(PSTR("\n некорректный # диска: %i"),dsk);
        return;
    }
    sbuf[0]=0;
    if (prefix != 0) {
        // открытие образа в подкаталоге
        strcpy(sbuf,Ffolder[dsk]);   // имя каталога с образами
        strcat(sbuf,"/");
    }
    strcat(sbuf,filename);
    fs_select(dsk);
    if (fs_open() != 0) {
        printf_P(PSTR("\nДиск %c: - нет файла %s, %i"),'A'+dsk,sbuf,lastError);
        fstatus[dsk]=0; // опускаем флаг - образ не смонтирован
        goto mnt_exit;
    }
    // читаем инфосектор файла

    //ESL    if (dsk != 4) {  // только для дисков A-D
    //EXRTOOLS теперь обычный KDI
    if (fs_read(fb, 32, &res) != 0) {
        printf_P(PSTR("\nДиск %c: не читается"),'A'+dsk,sbuf);
        fstatus[dsk]=0;
        goto mnt_exit;
    }

    lspt[dsk]=fb[16];       // достаем логический SPT
    if ((dsk == 0) && (prefix != 0)) systracks=fb[29]; // достаем число системных дорожек c диска А, если нет подстановки
    //ESL    }

    // получаем имя файла для вывода в терминал
    sbuf[0]=0;
    if (prefix == 1) {
        strcpy(sbuf,Ffolder[dsk]);   // имя каталога с образами
        strcat(sbuf,"/");
    }
    strcat(sbuf,filename);
    printf_P(PSTR("\n Mount %c: %s  SPT=%i"),'A'+dsk,sbuf,lspt[dsk]);
    if (prefix != 0) printf_P(PSTR(" SYST=%i"),systracks);
    printf_P(PSTR("\n"));

    fstatus[dsk]=1;      // взводим флаг - образ смонтирован успешно

    // Точка выхода
 mnt_exit:
    pflush();
}


//************************************
//*   Создание пустого образа диска
//************************************
void create_disk(unsigned char drv, unsigned char* filename) {

    int i;
    unsigned char* sbuf=getsbufptr();  // буфер сектора VinxFS

    fs_select(drv);
    strcpy(sbuf,diskfolder);
    strcat(sbuf,"/");
    strcat(sbuf,filename);
    fs_create();			// создаем пока пустой файл
    printf_P(PSTR("\nСоздание файла...\n"));

    // Этап 1 - Формируем буфер #0 - c инфосектором
    memset(fb,0xe5,128);         // заполнитель
    memcpy_P(fb,infosector,32);  // инфосектор
    // записываем буфер 0 - 0-100
    fs_write(fb,128);
    //memset(fb,0xe5,128);         // заполнитель
    //fs_write(fb,128);

    // Этап 2 - пустой каталог
    memset(fb,0xe5,32);    // Затираем данные инфосекторв в буфере-заполнителе
    fs_lseek(0x2800,0);    // Встаем на начало каталога - 2800
    // Заполняем область каталога заполнителем - 2800-3800
    for(i=0; i<0x20; i++) {
        fs_write(fb,128);
        printf_P(PSTR("\r%i"),i+1);
    }

    // Этап 3 - расширяем файл и записыываем сектор в хвост для резервирования места
    fs_lseek(0xc7f80,0);   // Полный размер файла - C8000
    fs_write(fb,128);
    fs_write_eof();
    //fs_lseek(0,0);   // На всякий случай встаем на начало файла


    // Записываем заполнитель во все секторы KDI
    fstatus[drv]=1;      // взводим флаг - образ создан и смонтирован

}




//*****************************************************************
//* Сохранение файла конфигурации
//*
//*  flags - сумма чисел, определяющих, что именно мы сохраняем:
//*     1 - образ А
//*     2 - образ В
//*     4 - образ C
//*     8 - образ D
//*    10 - каталог по умолчанию
//*
//*  preserve - сохранение предыдущего содержимого файла:
//*     0 - не сохранять - формируем дефлотные имена
//*     1 - сохранять
//*****************************************************************
void save_config(unsigned char flags, unsigned char preserve) {


    unsigned char* sbuf=getsbufptr();  // буфер сектора VinxFS
    int i,res,st;

    printf_P(PSTR("\nMOUNT.CFG: f=%x  p=%i"),flags,preserve);

    fs_select(5);   // блок для служебных файлов

    strcpy(sbuf,"MOUNT.CFG");    // файл списка монтирования
    res=-1;

    // читаем старый файл
    res=fs_open();
    //printf_P(PSTR(" res=%i err=%i"),res,lastError);
    if (res == 0)  fs_read(fb,126,&st);
    else  fs_create();     // файла нет - создаем

    // Значения по умолчанию. также отсутствии файла
    if ((preserve == 0)||(res!=0)) {
        // имена файлов по умолчанию
        printf_P(PSTR("\nКонфиг по умолчанию p=%i r=%i"),preserve,res);
        strcpy(Fname[0],"DISKA.KDI");
        strcpy(Fname[1],"DISKB.KDI");
        strcpy(Fname[2],"DISKC.KDI");
        strcpy(Fname[3],"DISKD.KDI");
        for (i=0; i<4; i++)  strcpy(Ffolder[i],"DISK");
        // каталог по умолчанию
        strcpy(diskfolder,Ffolder[0]);
    }


    // Формируем структуру будущего MOUNT.CFG в буфере FB и
    if ((flags&1) != 0) {
        // диск 0
        memcpy(fb,Ffolder[0],14);
        memcpy(fb+14,Fname[0],14);
    }
    if ((flags&2) != 0) {
        // диск 1
        memcpy(fb+28,Ffolder[1],14);
        memcpy(fb+42,Fname[1],14);
    }
    if ((flags&4) != 0) {
        // Диск 2
        memcpy(fb+56,Ffolder[2],14);
        memcpy(fb+70,Fname[2],14);
    }
    if ((flags&8) != 0) {
        // Диск 3
        memcpy(fb+84,Ffolder[3],14);
        memcpy(fb+98,Fname[3],14);
    }
    // Каталог по умолчанию
    if ((flags&0x10) != 0) {
        memcpy(fb+112,diskfolder,14);
    }
    // сохраняем новый конфиг
    fs_lseek(0,0);
    fs_write(fb,126);
    fs_write_eof();
    //dump(fb,126);
}


//*********************************************
//*  Передача каталога образов из $diskfolder/ *
//*********************************************
void send_dir() {

    unsigned char* sbuf=getsbufptr();  // буфер сектора VinxFS
    unsigned int i,res,cnt=0;

    fs_select(5);   // блок для служебных файлов
    strcpy(sbuf,diskfolder);
    fs_opendir();
    printf_P(PSTR("\nКаталог образов"));
    for(i=0;; i++) {
        res=fs_readdir();
        if (res != 0) {
            printf_P(PSTR("\n readdir #%i error: %04x - %04x"),i,res,lastError);
            break;
        }
        if(FS_DIRENTRY[0] == 0) break;  // конец каталога
        FS_DIRENTRY[DIR_Attr] = 0;
        if ((FS_DIRENTRY[0] != '.')&&(FS_DIRENTRY[DIR_Attr] != 0x10)) {   // пропускаем скрытые файлы и каталоги
            FS_DIRENTRY[DIR_Attr] = 0; // конец имени файла - ограничиваем 0
            printf_P(PSTR("\n%i: %s  %lu"), i,FS_DIRENTRY,*((unsigned long*)&FS_DIRENTRY[DIR_FileSize]));
            iop_sendbuf(FS_DIRENTRY,14); // отсылаем в корвет буфер с именем файла
            cnt++;
        }
    }
    iop_send_byte(0);  // конец каталога
    printf_P(PSTR("\n-- Всего %i файлов--"), cnt);
}

//*********************************************
//*  Передача списка каталогов карты          *
//*********************************************
void send_folders() {

    unsigned char* sbuf=getsbufptr();  // буфер сектора VinxFS
    unsigned int i,res,cnt=0;

    fs_select(5);   // блок для служебных файлов
    sbuf[0]=0;  // корень карты
    fs_opendir();
    printf_P(PSTR("\nСписок каталогов карты"));
    for(i=0;; i++) {
        res=fs_readdir();
        if (res != 0) {
            printf_P(PSTR("\n readdir #%i error: %04x - %04x"),i,res,lastError);
            break;
        }
        if(FS_DIRENTRY[0] == 0) break;  // конец каталога
        if ((FS_DIRENTRY[0] != '.')&&(FS_DIRENTRY[DIR_Attr] == 0x10)) {   // пропускаем скрытые каталоги и обычные файлы
            FS_DIRENTRY[DIR_Attr] = 0; // конец имени файла - ограничиваем 0
            printf_P(PSTR("\n%i: %s  %lu"), i,FS_DIRENTRY,*((unsigned long*)&FS_DIRENTRY[DIR_FileSize]));
            iop_sendbuf(FS_DIRENTRY,14); // отсылаем в корвет буфер с именем файла
            cnt++;
        }
    }
    iop_send_byte(0);  // конец каталога
    printf_P(PSTR("\n-- Всего %i каталогов--"), cnt);
}

//***************************************
//*  Установка каталога по умолчанию    *
//***************************************
void set_def_floder(unsigned char sec) {

    unsigned char* sbuf=getsbufptr();  // буфер сектора VinxFS
    unsigned char tmpname[14];

    fs_select(5);   // блок для служебных файлов
    iop_getbuf(tmpname,14);          // имя каталога
    strcpy(sbuf,tmpname);            // устанавливаем как имя открываемого каталога
    if (fs_opendir() == 0) {
        // каталог открыт - все ок
        printf_P(PSTR("\nDefdir: %s"),tmpname);
        strcpy(diskfolder,tmpname);     // копируем его имя как каталог по умолчанию
        if (sec == 1) save_config(0x10,1);   // если надо - сохраняем на будущее
    }
    else printf_P(PSTR("\n Каталог %s не найден"),tmpname);
}



//**********************************************************************************************
//* Int1 - от сигнала -OBF
//*
//*  В этом прерывании обрабатываются запросы, поступающие от корвета
//*
//*  Все запросы начинаются 1-байтовым кодом команды, за которым следует пакет параметров из 4 байт.
//*--------------------------------------------------------
//*
//*  Команды обмена секторами данных:
//*
//*  CMD:    DS    1    // код команды: 01-чтение  02-запись
//*  DRV:    DS    1    // номер устройства, 0-A,  1-B, 2-C,  3-D
//*  TRK:    DS    1    // номер дорожки
//*  SEC     DS    1    // номер сектора (размер сектора всегда 128 байт
//*  CSUM:   DS    1    // Контрольная сумма предыдущих 4 байт, равна их беззнаковой 8-битной сумме минус 1:  CSUM=(CMD+DRV+TRK+SEC)&0xFF-1
//*
//*  Команды измерителя скорости. Все поля пакета, кроме кода команды, не имеют значения.
//*
//*   F0 - прием 8000h байта мусора
//*   F1 - передача 8000h байтов мусора
//*
//*  Комадны управления работой контроллера
//*
//*   00 - пустая операция, всегда возвращает ответ ОК (1)
//*   A0 - включить/выключить подстановку системных дорожек. DRV всегда 0, TRK=0 - выключить, 1 - включить
//*   A1 - включить/выключить реакцию на control: TRK=0 - выключить, TRK=1 - включить
//*
//*  Команды управления образами дисков
//*
//*   80 - получить имя файла образа, связанного с логическим диском A или В
//*         DRV - определяет, для какого из дисков выполняется запрос
//*         После приема команды и ответа ОК контроллер передает:
//*            1 байт флаг R/O (0 или 1)
//*            массив из 14 байт с именем каталога
//*            массив из 14 байт с именем файла
//*
//*   81 - монтировать образ к логическому диску
//*         DRV - определяет, для какого из дисков выполняется запрос
//*         TRK - 0-чтение/запись  1-только чтение
//*         SEC - 0-временно  1-навсегда
//*         После приема команды и ответа ОК контроллер принимает массив из 14 байт с именем файла
//*
//*   82 - узнать состояние монтирования образа
//*         DRV - определяет, для какого из дисков выполняется запрос
//*         Команда возвращает ответ 0 (error), если диск не смонтирован, или не 0 (OK), если смонтирован
//*
//*   83 - создать пустой KDI-файл
//*         DRV - указывает, к какому диску будет примонтирован создаваемый KDI-образ
//*
//*   84 - получить список файлов-образов из каталога $diskfolder карты
//*         Параметров команда не имеет.
//*         Возвращаются строки по 14 байт с именами файлов
//*         После всех файлов передается 00 (строки файлов с нуля начинаться не могут).
//*
//*   85 - Получить имя каталога для вновь монтируемых образов
//*         Возвращаются 14 байт с именем каталога
//*
//*   86 - Установить имя каталога для вновь монтируемых образов
//*         SEC - 0-временно  1-навсегда
//*         После приема команды контрллер принимает 14 байт с именем каталога
//*
//*   87 - получить список каталогов, имеющихся на карте
//*         Параметров команда не имеет.
//*         Возвращаются строки по 14 байт с именами каталогов
//*         В конце списка передается 00 (строки с нуля начинаться не могут).
//*
//*   88 - Снять защиту записи с инструментального диска E
//**********************************************************************************************
ISR (INT1_vect) {

    unsigned char cmd,drv,trk,sec,csum,rcsum;  // байты командного пакета
    unsigned long offset;   // смещение до сектора от начала образа файла
    unsigned int i,res;

    // Проверяем условия вызова прерывания - бывают и ложные.

    if ((bit_is_clear(PIND,2))          // нажата кнопка RESET, Control ушел в 0, ничего не делаем.
            ||(bit_is_set(PIND,3))) return;  // или нет запроса прерывания - ложная тревога


    GICR&=~_BV(INT1);         // Запрещаем повторный вход в прерывание
    sei();                    // открываем прерывания - надо обрабатывать консоль и ждать Control
    PORTB&=~_BV(0);           // светодиод включить

    // получаем код команды
    cmd=iop_get_byte();

    // получаем параметры команды
    drv=iop_get_byte();
    trk=iop_get_byte();
    sec=iop_get_byte();
    csum=iop_get_byte();


    // Отладночный лог команд. Включать только по необходимости, так как резко снижается производительность

    //pflush();
    //printf_P(PSTR("\ncmd %x %x %x %x"),cmd,drv,trk,sec);
    //pflush();

    // Проверка контрольной суммы
    rcsum=cmd+drv+trk+sec-1;
    if (rcsum != csum) {
        printf_P(PSTR("\nCS ERR: r:%x c:%x"),rcsum,csum);
        goto nocmd;
    }

    // проверка допустимости номера диска
    if (drv >4) {
        printf_P(PSTR("\nDRV ERR: %x"),drv);
        iop_send_byte(0);   // ответ Error
        goto nocmd;
    }

    //================== Разбор и исполнение команд =========================================
    switch (cmd) {
        // команда NOP
    case 0:
        UDR='0';  // выводим знак записи
        iop_send_byte(1);   // ответ ОК
        break;
    //-------------------------------------------------------------------------------------
    case 1:
    case 2:
        // ------ чтение/запись сектора -----
        // Подмена образа диска на образ BIOS - только для образа 0 и только если разрешено
        if ((drv == 0) && (enable_bios_substitute_flag == 1)) {
            if ((trk<systracks) && (bios_flag == 0)) {
                mount_disk(0,sysfiles[substitute_number],0);    // переход на подставную систему
                bios_flag=1;
            }
            if ((trk>=systracks) && (bios_flag == 1)) {
                mount_disk(0,Fname[0],1);    // переход на реальный диск A
                bios_flag=0;
            }
        }
        fs_select(drv);   // выбираем блок с открытым файлом
        // проверяем, смонтирован ли образ
        if (fstatus[drv] == 0) {
            iop_send_byte(0);     // не смонтирован - ответ ERROR
            break;
        }
        // вычисляем смещение до сектора
        offset=(trk*lspt[drv]+sec);  // смещение в блоках
        fs_lseek(offset*128,0);      // при позиционировании передаем смещение в секторах
        if (cmd == 1) {
            // ******** операция чтения ************
            UDR='R';  // выводим знак чтения
            // Читаем блок из файла и проверяем на ошибки
            if (fs_read(fb,128,&res) != 0)  {
                iop_send_byte(0);       // ответ ERROR
                break;
            }
            iop_send_byte(1);          // ответ ОК
            iop_sendbuf(fb,128);       // отправляем блок в корвет
        }
        else {
            // ******** операция записи ************
            UDR='W';  // выводим знак записи
            // проверяем готовность диска и защиту записи
            if ((fstatus[drv] == 0)||(roflag[drv] == 1)) {
                iop_send_byte(0);   // ответ Error
                break;
            }
            iop_send_byte(1);     // ответ ОК
            iop_getbuf(fb,128);   // получаем блок из корвета
            if ((drv != 0) || (bios_flag == 0) || (enable_bios_substitute_flag == 0))  // запись в подставной файл запрещена
                fs_write(fb,128); // на всякий случай запись лучше выключить при отладке - иначе можно испортить KDI при ошибках позиционирования
        }
        // ****** точка выхода при ошибках ********
        break;

    //-------------------------------------------------------------------------------------
    case 0xa0:
        // --------- включение-отключение подстановки системных дорожек
        if (drv == 0) {
            iop_send_byte(1);
            if (trk == 0) enable_bios_substitute_flag=0;
            else {
                enable_bios_substitute_flag=1;
                sysfiles[2][6]=trk;    // впиывваем # в SYSTEMn.BIN
                substitute_number=trk-1; // индекс в массиве подставляемых имен файлов
                if (substitute_number>2) substitute_number=2;  // для параметров 3 и более подставляем SYSTEMn.BIN
                //esl
                systracks=(substitute_number == 1) ? 3 : 2 ;// 3 track for microdos, 2 for other
            }
        }
        else  iop_send_byte(0);   // иначе ответ Error
        UDR='S';
        break;


    //-------------------------------------------------------------------------------------
    case 0xa1:
        // --------- включение-отключение реакции на contol
        UDR='K';
        iop_send_byte(0);   // иначе ответ Error
        if (trk == 0) GICR&=~_BV(INT0);   // полное отключение реакции на Control
        else GICR|=_BV(INT0);             // Разрешаем Int0 от Control
        break;


    //-------------------------------------------------------------------------------------
    case   0x80:
        // Получение имени файла-образа
        UDR='F';
        iop_send_byte(1);           // ответ ОК
        iop_send_byte(roflag[drv]); // отправляем флаг только чтение
        iop_sendbuf(Ffolder[drv],14); // отправляем имя каталога
        iop_sendbuf(Fname[drv],14); // отправляем имя файла
        break;

    //-------------------------------------------------------------------------------------
    case   0x81:
        // Монтирование файла-образа
        UDR='M';
        iop_send_byte(1);           // ответ ОК
        iop_getbuf(Fname[drv],14);  // принимаем имя файла
        roflag[drv]=trk;            // флаг разрешения записи
        strcpy(Ffolder[drv],diskfolder);  // Устанавливаем каталог, в которм ищем открываемый образ
        if(sec == 1) save_config((1<<drv),1);  // сохраняем конфиг, если надо
        mount_disk(drv,Fname[drv],1);     // монтируем диск
        break;

    //-------------------------------------------------------------------------------------
    case   0x82:
        // Получение состояния монтирования образа
        UDR='Q';
        iop_send_byte(fstatus[drv]);   // ответ 0 или 1
        break;

    //-------------------------------------------------------------------------------------
    case   0x83:
        // Создание пустого образа диска и монтирование его
        UDR='C';
        iop_send_byte(1);             // ответ ОК
        iop_getbuf(Fname[drv],14);    // принимаем имя файла
        create_disk(drv,Fname[drv]);  // создаем образ диска
        mount_disk(drv,Fname[drv],1); // монтируем диск
        break;

    //-------------------------------------------------------------------------------------
    case   0x84:
        // Получение списка файлов из $diskfolder
        UDR='D';
        iop_send_byte(1);   // ответ ОК
        send_dir();
        break;

    //-------------------------------------------------------------------------------------
    case   0x85:
        // Получение имени каталога по умолчанию
        UDR='U';
        iop_send_byte(1);   // ответ ОК
        iop_sendbuf(diskfolder,14);
        break;

    //-------------------------------------------------------------------------------------
    case   0x86:
        // установка каталога по умолчанию
        UDR='J';
        iop_send_byte(1);             // ответ ОК
        set_def_floder(sec);            // устанавливаем каталог по умолчанию
        break;

    //-------------------------------------------------------------------------------------
    case   0x87:
        // Получение списка каталогов карты
        UDR='/';
        iop_send_byte(1);   // ответ ОК
        send_folders();
        break;

    //-------------------------------------------------------------------------------------
    case   0x88:
        // снятие защиты записи с диска Е
        UDR='|';
        iop_send_byte(1);   // ответ ОК
        roflag[4]=0;
        break;

    //-------------------------------------------------------------------------------------
    case 0xf1:
        // ------------ измеритель скорости - прием 32к мусора
        UDR='Z';  // выводим знак теста приема
        iop_send_byte(1);   // ответ ОК
        for(i=0; i<0x8000; i++) iop_get_byte(); // получаем мусор из корвета и отбрасываем
        GIFR|=_BV(INTF1);	// сбрасываем ложное ждущее прерывание от обмена данными
        break;

    //-------------------------------------------------------------------------------------
    case 0xf0:
        // ------------ измеритель скорости - передача 32к мусора
        UDR='U';  // выводим знак теста передачи
        iop_send_byte(1);   // ответ ОК
        for(i=0; i<0x8000; i++) iop_send_byte(0xeb); // отсылаем мусор в корвет
        break;


    //-------------------------------------------------------------------------------------
    default:
        printf_P(PSTR("\n? %x "),cmd); // неопределенная команда
        iop_send_byte(0);   // ответ ERROR
        break;
    }

    // Сюда идет обход при неправильной контрольеой сумме пакета
 nocmd:

    PORTB|=_BV(0); 	        // светодиод отключить
    cli();                  // запрещаем прерывания во избежании повторного входа в этот обработчик
    GICR|=_BV(INT1);        // Разрешаем Int1 от -OBF ВВ55
}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//###########   Головная программа       ###########################

void main(void) {

    unsigned int i;
    unsigned int res;
    unsigned char st;
    unsigned char infobuf[16];
    unsigned char* sbuf;
    unsigned char reset_config=0;

    //----- настройка собаки
    wdt_disable();					// собака пока не нужна

    //---------- прерывания

    MCUCR= _BV(ISC00)|_BV(ISC11);	// INT0 - Прерывание по обоим фронтам   Int1 - только по отрицательному фронту
    GICR=_BV(INT0);                 // Разрешаем Int0

    //------ Настройка портов GPIO

    DDRA=0;          // порт данных пока на ввод
    PORTA=0;	 // отключаем подтяжки
    DDRD=_BV(1);     // TxD на вывод
    DDRB=_BV(0)|_BV(4)|_BV(5)|_BV(7);  // индикатор, SS, MOSI, SCK - на вывод
    PORTB=_BV(0)|_BV(3)|_BV(4);        // SS=1, светодиод отключить, включить подтяжку кнопки


    //--- настройка последовательного порта

    //Скорость обмена UART - 1000000 или 115200, с учетом удвоителя U2X

    UBRRH=0;
    #ifdef LOWSPEED
    UBRRL=8;  // 115200
    #else
    UBRRL=0;  // 1000000
    #endif
    UCSRA=(1<< U2X);
    // Включаем приемник, передатчик и открываем прерывание от них
    UCSRB = (1 << RXCIE)|(1<<UDRIE)|(1 << RXEN)|(1 << TXEN );
    // Формат кадра - 8N1
    UCSRC = (1<<URSEL)|(1<<UCSZ1)|(1<<UCSZ0);

    // Инициализируем маркеры буферов
    USART_RxTail=0;
    USART_RxHead=0;
    USART_TxTail=0;
    USART_TxHead=0;

    // Назначаем поток вывода
    stdout = &mystdout;


    //---------- Настройка контроллера SPI --------------------


    SPCR =_BV(SPE)|_BV(MSTR); // разрешаем SPI, режим master, максимальная скорость
    SPSR|= _BV(SPI2X);		   // удваиваем скорость обмена - глюкает с картой, поэтому пока не включаем

    //-----------------------------------------------------------------------------
    // Открываем прерывания. Настройка закончена, входим в рабочий режим
    //-----------------------------------------------------------------------------

    boot_flag=0;  // корвет пока не загружен

    sei();


    // Инициализация SD-карты

    _delay_ms(10);  // задержка на запуск карты

    printf_P(PSTR("\n\n   *** Extrom - SD version:%s***\n\n"),VERSION);

    printf_P(PSTR("\nИнициализация SD-карты"));

    PORTB|=0x10;   // Поднимаем сигнал SS
    for(i=0; i<9; i++) spi_send(0xff); // 88 перепадов SCK - для ввода карты в SPI-режим
    PORTB&=0xef;   // Опускаем сигнал SS. Навсегда - карта всегда выбрана

    // Отсылаем команду CMD0 и проверяем готовность карты
    st=send_sd_cmd (0,0);
    spi_send (0xff); // межкомандный промежуток

    // Попытка отослать CMD1
    for(i=0; i<0xffff; i++) {
        st=send_sd_cmd (1,0);	   // CMD1
        spi_send (0xff);         // межкомандный промежуток
        if (st == 0) break;
    }

    // Идентифицируем карту

    res=send_sd_cmd(10,0);   // команда чтения CID
    if (res != 0) printf_P(PSTR("\nОшибка идентификации - %2x"),i);

    // Получаем блок CID
    if (read_sd_data(infobuf,16,0) != 0) {
        printf_P(PSTR("\nSD карта не найдена!"));
        led_error(5);
    }
    printf_P(PSTR("\nCARD MID=%2x  OID=%2x"),(unsigned int)infobuf[0],*((unsigned int*)&infobuf[1]));
    printf_P(PSTR("\nProduct - "));
    printstrl(infobuf+3,5);
    printf_P(PSTR("\nrev %i  serail %i"),(unsigned int)infobuf[8],*((unsigned int*)&infobuf[9]));

    sbuf=getsbufptr();  // получаем указатель на буфер сектора

    res=fs_init(); // монтируем файловую систеу на карте
    if (res != 0) {
        printf_P(PSTR("\nНет файловой системы:%i"),lastError);
        led_error(3);
    }

    sbuf[0]=0;   // корень ФС
    fs_select(5);   // блок для служебных файлов
    res=fs_opendir();
    if (res != 0) {
        printf_P(PSTR("\nОшибки в корневом каталоге:%02x"),res);
        led_error(4);
    }

    printf_P(PSTR("\n\n -- Корневой каталог карты --\n"));
    for(i=0; i<20; i++) { // не более 20 файлов
        res=fs_readdir();
        if(FS_DIRENTRY[0] == 0) break;  // конец каталога
        if (res != 0) printf_P(PSTR("\n readdir #%i error:%04x"),i,lastError);
        else {
            st=FS_DIRENTRY[DIR_Attr];  // атрибут файла
            FS_DIRENTRY[DIR_Attr] = 0; // конец имени файла - ограничиваем 0
            if (st == 0x10)  // каталог
                printf_P(PSTR("\n%i: %s   <DIR>"), i,FS_DIRENTRY);
            else    // регулярный файл
                printf_P(PSTR("\n%i: %s  %lu"), i,FS_DIRENTRY,*((unsigned long*)&FS_DIRENTRY[DIR_FileSize]));
        }
    }
    printf_P(PSTR("\n\nВсего %i файлов\n"), i);

    if ((PINB&8) == 0) reset_config=1; // нажата кнопка сброса конфигурации
    if (reset_config != 0) printf_P(PSTR("\nСброс конфигурации"));

    //------------ Запуск загрузки 2 фазы --------------------------

    PORTB|=_BV(0); 	        // светодиод отключить

    //ESL
    printf_P(PSTR("\nОжидание загрузки Stage1 в Корвет"));

    while(boot_flag == 0) {}; // ждем окончания начальной загрузки корвета
    DDRD=0x52;              // рабочий режим - TxD,-STB, -ACK - на вывод
    PORTD=0x50;		// -STB=1    -ACK=1

    // заливаем и запускаем загрузчик 2 ступени
    send_loader();

    printf_P(PSTR("\nМонтирования дисков"));
    // разбираем файл конфигурации
    fs_select(5);   // блок для служебных файлов
    strcpy(sbuf,"MOUNT.CFG");    // открываем файл конфигурации
    if (reset_config == 0) res=fs_open();
    if ((reset_config != 0)|| (res != 0)) {
        if (reset_config != 0) {
            printf_P(PSTR("\nНажата кнопка сброса конфиурации"));
        }
        printf_P(PSTR("\nСоздание дефолтной конфигурации монтировании дисков"));
        save_config(0x1f,0);   // файла еще нет или нажата кнопка - записываем в mount.cfg
    }
    else {  // файл есть - читаем его и распихиваем по переменным
        fs_read(fb,126,&res);
        // Диск 0
        memcpy(Ffolder[0],fb,14);
        memcpy(Fname[0],fb+14,14);
        // Диск 1
        memcpy(Ffolder[1],fb+28,14);
        memcpy(Fname[1],fb+42,14);
        // Диск 2
        memcpy(Ffolder[2],fb+56,14);
        memcpy(Fname[2],fb+70,14);
        // Диск 3
        memcpy(Ffolder[3],fb+84,14);
        memcpy(Fname[3],fb+98,14);
        // Каталог по умолчанию
        memcpy(diskfolder,fb+112,14);
    }
    printf_P(PSTR("\nDefdir: %s"),diskfolder);

    // Монтируем начальные образы ко всем дискам
    for(i=0; i<4; i++) {   // Для конфигурации без реальных дисководов заменить 2 на 4 !!!!!!!!!!!!1
        printf_P(PSTR("\nDisk %c: %s/%s"),'A'+i,Ffolder[i],Fname[i]);
        mount_disk(i,Fname[i],1);
        roflag[i]=0;    // снимаем флаги Readonly
    }

    // Монтируем инструментальный диск E

    //ESL
    //    lspt[4]=15;    // фиксированное SPT для mount-диска
    //    mount_disk(4,"EXRTOOLS.DSK",0); // mount-диск
    // проинициализируктся в mount_disk // lspt[4]=15;    // фиксированное SPT для mount-диска
    mount_disk(4,"EXRTOOLS.KDI",0); // mount-диск
    //ESL
    roflag[4]=1;   // запись по умолчанию запрещена

    // Далее идет бесконечный цикл ожидания перезагрузки корвета. Вся полезная работа будет идти в прерывании int1
    GICR|=_BV(INT1);         // Разрешаем Int1 от -OBF ВВ55

    for(;;) {}  // Из этого цикла выхода нет - работают только обработчики прерываний
}
