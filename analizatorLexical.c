#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>

enum{
    //constante si ID
    ID, CT_INT, CT_REAL, CT_CHAR, CT_STRING,
    //cuvinte cheie
    BREAK, CHAR, DOUBLE, ELSE, IF, FOR, INT, RETURN, STRUCT, VOID, WHILE,
    //delimitatori
    COMMA, SEMICOLON, LPAR, RPAR, LBRACKET, RBRACKET, LACC, RACC, END,
    //operatori
    ADD, SUB, MUL, DIV, DOT, AND, OR, NOT, ASSIGN, EQUAL, NOTEQ, LESS, LESSEQ, GREATER, GREATEREQ
};

char *map[] = {
    "ID", "CT_INT", "CT_REAL", "CT_CHAR", "CT_STRING",
    "BREAK", "CHAR", "DOUBLE", "ELSE", "IF", "FOR", "INT", "RETURN", "STRUCT", "VOID", "WHILE",
    "COMMA", "SEMICOLON", "LPAR", "RPAR", "LBRACKET", "RBRACKET", "LACC", "RACC", "END",
    "ADD", "SUB", "MUL", "DIV", "DOT", "AND","OR", "NOT", "ASSIGN", "EQUAL", "NOTEQ", "LESS", "LESSEQ", "GREATER", "GREATEREQ"
};

void err(const char *fmt,...)
{
  va_list va;
  va_start(va,fmt);
  fprintf(stderr,"error: ");
  vfprintf(stderr,fmt,va);
  fputc('\n',stderr);
  va_end(va);
  exit(-1);
}

#define SAFEALLOC(var,Type) if((var=(Type*)malloc(sizeof(Type)))==NULL)err("Not enough memory");
#define SAFEALLOCN(var,Type,n) if((var=(Type*)malloc(n*sizeof(Type)))==NULL)err("Not enough memory");

int line;
char bufin[30000];
char *pCrtCh;

typedef struct _Token{
  int code; // codul (numele)
  union{
    char *text; // folosit pentru ID, CT_STRING (alocat dinamic)
    int i; // folosit pentru CT_INT, CT_CHAR
    double r; // folosit pentru CT_REAL
  };
  int line; // linia din fisierul de intrare
  struct _Token *next; // inlantuire la urmatorul AL
}Token;

void tkerr(const Token *tk,const char *fmt,...)
{
  va_list va;
  va_start(va,fmt);
  fprintf(stderr,"error in line %d: ",tk->line);
  vfprintf(stderr,fmt,va);
  fputc('\n',stderr);
  va_end(va);
  exit(-1);
}

Token *tokens, *lastToken;

Token *addTk(int code)
{
  Token *tk;
  SAFEALLOC(tk,Token)
  tk->code = code;
  tk->line = line;
  tk->next = NULL;
  if(lastToken){
    lastToken->next = tk;
  }
  else{
    tokens = tk;
  }
  lastToken = tk;
  return tk;
}

char *createString(const char *pStartCh, const char *pCrtCh)
{
    size_t n = (pCrtCh - pStartCh);
    char *p;
    SAFEALLOCN(p, char, n+1);
    memcpy(p, pStartCh, n);
    p[n] = '\0';
    return p;
}

int getNextToken()
{
  int state=0,nCh;
  char ch;
  const char *pStartCh;
  Token *tk;
  
  for(;;){// bucla infinita
    ch=*pCrtCh;
    switch(state){
      case 0:
        if(isalpha(ch)||ch=='_'){
          pStartCh=pCrtCh;// memoreaza inceputul ID-ului
          pCrtCh++;// consuma caracterul
          state=1;// trece la noua stare
        }
        else if(ch=='\''){
          pCrtCh++;
          pStartCh = pCrtCh;
          state=3;
        }
        else if(ch=='"'){
          pCrtCh++;
          pStartCh = pCrtCh;
          state=6;
        }
        else if(isdigit(ch)){
          pStartCh = pCrtCh;
          pCrtCh++;
          state=9;
        }
        else if(ch==','){
          pCrtCh++;
          state=17;
        }
        else if(ch==';'){
          pCrtCh++;
          state=18;
        }
        else if(ch=='('){
          pCrtCh++;
          state=19;
        }
        else if(ch==')'){
          pCrtCh++;
          state=20;
        }
        else if(ch=='['){
          pCrtCh++;
          state=21;
        }
        else if(ch==']'){
          pCrtCh++;
          state=22;
        }
        else if(ch=='{'){
          pCrtCh++;
          state=23;
        }
        else if(ch=='}'){
          pCrtCh++;
          state=24;
        }
        else if(ch=='\0' || ch==EOF){
          pCrtCh++;
          state=25;
        }
        else if(ch=='+'){
          pCrtCh++;
          state=26;
        }
        else if(ch=='-'){
          pCrtCh++;
          state=27;
        }
        else if(ch=='*'){
          pCrtCh++;
          state=28;
        }
        else if(ch=='/'){
          pCrtCh++;
          state=29;
        }
        else if(ch=='.'){
          pCrtCh++;
          state=32;
        }
        else if(ch=='&'){
          pCrtCh++;
          state=33;
        }
        else if(ch=='|'){
          pCrtCh++;
          state=35;
        }
        else if(ch=='='){
          pCrtCh++;
          state=37;
        }
        else if(ch=='!'){
          pCrtCh++;
          state=40;
        }
        else if(ch=='<'){
          pCrtCh++;
          state=43;
        }
        else if(ch=='>'){
          pCrtCh++;
          state=46;
        }
        else if(ch==' '||ch=='\r'||ch=='\t'){
          pCrtCh++;
        }
        else if(ch=='\n'){ // tratat separat pentru a actualiza linia curenta
          line++;
          pCrtCh++;
        }
        else tkerr(addTk(END),"caracter invalid");
        break;
      case 1:
        if(isalnum(ch)||ch=='_') 
          pCrtCh++;
        else state=2;
        break;
      case 2:
        nCh=pCrtCh-pStartCh; // lungimea cuvantului gasit
        // teste cuvinte cheie
        if(nCh==5&&!memcmp(pStartCh,"break",5)) tk=addTk(BREAK);
        else if(nCh==4&&!memcmp(pStartCh,"char",4)) tk=addTk(CHAR);
        else if(nCh==6&&!memcmp(pStartCh,"double",6)) tk=addTk(DOUBLE);
        else if(nCh==4&&!memcmp(pStartCh,"else",4)) tk=addTk(ELSE);
        else if(nCh==3&&!memcmp(pStartCh,"for",3)) tk=addTk(FOR);
        else if(nCh==2&&!memcmp(pStartCh,"if",2)) tk=addTk(IF);
        else if(nCh==3&&!memcmp(pStartCh,"int",3)) tk=addTk(INT);
        else if(nCh==6&&!memcmp(pStartCh,"return",6)) tk=addTk(RETURN);
        else if(nCh==6&&!memcmp(pStartCh,"struct",6)) tk=addTk(STRUCT);
        else if(nCh==5&&!memcmp(pStartCh,"while",5)) tk=addTk(WHILE);
        else{ // daca nu este un cuvant cheie, atunci e un ID
          tk=addTk(ID);
          tk->text=createString(pStartCh,pCrtCh);
        }
        return tk->code;
      case 3:
        if(ch!='\''){
          pCrtCh++;
          state=4;
        }
        else tkerr(addTk(END),"caracter invalid");
        break;
      case 4:
        if(ch=='\''){
          pCrtCh++;
          state=5;
        }
        else tkerr(addTk(END),"caracter invalid");
        break;
      case 5:
        tk=addTk(CT_CHAR);
        tk->i = pStartCh[0]-'0'; 
        return CT_CHAR;
      case 6:
        if(ch!='"')
          pCrtCh++;
        else{
          pCrtCh++;
          state=8;
        }
        break;
  /**    case 7:
        if(ch=='"'){
          pCrtCh++;
          state=8;
        }
        break; **/
      case 8:
        tk=addTk(CT_STRING);
        tk->text = createString(pStartCh,pCrtCh-1);
        return CT_STRING;
      case 9:
        if(isdigit(ch)){
          pCrtCh++;
        }
        else if(ch=='.'){
          pCrtCh++;
          state=11;
        }
        else if(ch=='e' || ch=='E'){
          pCrtCh++;
          state=13;
        }
        else
          state=10;
        break;
      case 10:
        tk = addTk(CT_INT);
        tk->i = atoi(createString(pStartCh,pCrtCh));
        return(CT_INT);
      case 11:
        if(isdigit(ch)){
          pCrtCh++;
          state=12;
        }
        else tkerr(addTk(END),"caracter invalid");
        break;
      case 12:
        if(isdigit(ch)){
          pCrtCh++;
        }
        else if(ch=='e' || ch=='E'){
          pCrtCh++;
          state=13;
        }
        else
          state=16;
        break;
      case 13:
        if(ch=='+' || ch=='-'){
          pCrtCh++;
          state=14;
        }
        else
          state=14;
        break;
      case 14:
        if(isdigit(ch)){
          pCrtCh++;
          state=15;
        }
        else tkerr(addTk(END),"caracter invalid");
        break;
      case 15:
        if(isdigit(ch)){
          pCrtCh++;
        }
        else
          state=16;
        break;        
      case 16:
        tk=addTk(CT_REAL);
        tk->r = atof(createString(pStartCh,pCrtCh));
        return CT_REAL;
      case 17:
        addTk(COMMA);
        return COMMA;
      case 18:
        addTk(SEMICOLON);
        return SEMICOLON;
      case 19:
        addTk(LPAR);
        return LPAR;
      case 20:
        addTk(RPAR);
        return RPAR;
      case 21:
        addTk(LBRACKET);
        return LBRACKET;
      case 22:
        addTk(RBRACKET);
        return RBRACKET;
      case 23:
        addTk(LACC);
        return LACC;
      case 24:
        addTk(RACC);
        return RACC;
      case 25:
        addTk(END);
        return END;
      case 26:
        addTk(ADD);
        return ADD;
      case 27:
        addTk(SUB);
        return SUB;
      case 28:
        addTk(MUL);
        return MUL;
      case 29:
        if(ch=='/'){
          pCrtCh++;
          state=31;
        }
        else
          state=30;
        break; 
      case 30:
        addTk(DIV);
        return DIV;
      case 31:
        if(ch!='\r' && ch!='\0' && ch!='\n'){
          pCrtCh++;
        }
        else if(ch=='\r' || ch=='\0'){
          pCrtCh++;
          state=0;
        }
        else if(ch=='\n'){
          line++;
          pCrtCh++;
          state=0;
        }
        break; 
      case 32:
        addTk(DOT);
        return DOT;
      case 33:
        if(ch=='&'){
          pCrtCh++;
          state=34;
        }
        else tkerr(addTk(END),"caracter invalid");
        break; 
      case 34:
        addTk(AND);
        return AND;
      case 35:
        if(ch=='|'){
          pCrtCh++;
          state=36;
        }
        else tkerr(addTk(END),"caracter invalid");
        break;   
      case 36:
        addTk(OR);
        return OR;
      case 37:
        if(ch=='='){
          pCrtCh++;
          state=39;
        }
        else
          state=38;
        break;   
      case 38:
        addTk(ASSIGN);
        return ASSIGN;
      case 39:
        addTk(EQUAL);
        return EQUAL;
      case 40:
        if(ch=='='){
          pCrtCh++;
          state=42;
        }
        else
          state=41;
        break; 
      case 41:
        addTk(NOT);
        return NOT;
      case 42:
        addTk(NOTEQ);
        return NOTEQ;
      case 43:
        if(ch=='='){
          pCrtCh++;
          state=45;
        }
        else
          state=44;
        break;    
      case 44:
        addTk(LESS);
        return LESS;
      case 45:
        addTk(LESSEQ);
        return LESSEQ;
      case 46:
        if(ch=='='){
          pCrtCh++;
          state=48;
        }
        else
          state=47;
        break;
      case 47:
        addTk(GREATER);
        return GREATER;
      case 48:
        addTk(GREATEREQ);
        return GREATEREQ;
      default:
        err("Eroare: Caracter necunoscut");
        break;
    }
  }
}

void showAtoms()
{
    Token *tk = tokens;
    while(tk)
    {
        printf("%d %s", tk->line+1, map[tk->code]);       
        if(tk->code == ID || tk->code == CT_STRING)
            printf(":%s", tk->text);
        else if(tk->code == CT_INT)
            printf(":%d", tk->i);
        else if(tk->code == CT_CHAR)
            printf(":%c", tk->i +'0');
        else if(tk->code == CT_REAL)
            printf(":%f", tk->r);

        printf("\n");

        tk = tk->next;
    }
}

int main()
{
  FILE *fis;
  fis = fopen("test.c", "rb");

  if (fis == NULL){
    printf("nu s-a putut deschide fisierul");
    return -1;
  }

  int n = fread(bufin, 1, 30000, fis);
  bufin[n] = '\0';
  fclose(fis);
  pCrtCh = bufin; // initializarea pch pe prima pozitie din buffin

  while(getNextToken() != END){}

  showAtoms();

  free(tokens);
  free(lastToken);

  return 0;
}