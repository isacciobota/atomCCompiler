#include "ad.h"
#include "at.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>

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

bool structDef();
bool fnDef();
bool varDef();
bool typeBase(Type* t);
bool arrayDecl(Type* t);
bool expr(Ret *r);
bool fnParam();
bool stmCompound(bool newDomain);
bool stm();
bool exprAssign(Ret *r);
bool exprUnary(Ret *r);
bool exprOr(Ret *r);
bool exprOrPrim(Ret *r);
bool exprAnd(Ret *r);
bool exprAndPrim(Ret *r);
bool exprEq(Ret *r);
bool exprEqPrim(Ret *r);
bool exprRel(Ret *r);
bool exprRelPrim(Ret *r);
bool exprAdd(Ret *r);
bool exprAddPrim(Ret *r);
bool exprMul(Ret *r);
bool exprMulPrim(Ret *r);
bool exprCast(Ret *r);
bool exprPostfix(Ret *r);
bool exprPostfixPrim(Ret *r);
bool exprPrimary(Ret *r);
Symbol* owner;

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

Token *tokens, *lastToken, *iTk, *consumedTk;

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
        else if(nCh==4&&!memcmp(pStartCh,"void",4)) tk=addTk(VOID);
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
  free(tk);
}

bool consume(int code){
    printf("consume(%s)",map[code]);
    if(iTk->code==code)
    {
        consumedTk=iTk;
        iTk=iTk->next;
        printf(" => consumed\n");
        return true;
    }
    printf(" => found %s\n",map[iTk->code]);
    return false;
}

bool unit(){
    Token *start=iTk;
    pushDomain();
    for(;;){
        if(structDef()){}
        else if(fnDef()){}
        else if(varDef()){}
        else break;
    }
    if(consume(END)){
        printf("\n\n\n");
        showDomain(symTable, "global");
        dropDomain();
        return true;
    }
    iTk = start;
    return false;
}

bool structDef(){
    Token *start = iTk;
    if(consume(STRUCT)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            if(consume(LACC)){
                Symbol *s=findSymbolInDomain(symTable,tkName->text);
                if(s)
                    tkerr(iTk,"symbol redefinition: %s",tkName->text);
                s=addSymbolToDomain(symTable,newSymbol(tkName->text,SK_STRUCT));
                s->type.tb=TB_STRUCT;
                s->type.s=s;
                s->type.n=-1;
                pushDomain();
                owner=s;
                for(;;){
                    if(varDef()){}
                    else break;
                }
                if(consume(RACC)){
                    if(consume(SEMICOLON)){
                        owner=NULL;
                        dropDomain();
                        return true;
                    }
                    else tkerr(iTk, "Need: ;");
                }
                else tkerr(iTk, "Need: }");
            }
        }
        else tkerr(iTk, "Need identifier after struct");
    }
    iTk = start;
    return false;
}

bool varDef(){
    Token *start = iTk;
    Type t;
    if(typeBase(&t)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            if(arrayDecl(&t)){
                if(t.n==0)
                    tkerr(iTk,"a vector variable must have a specified dimension");
            }
            if(consume(SEMICOLON)){
                Symbol *var=findSymbolInDomain(symTable,tkName->text);
                if(var)
                    tkerr(iTk,"symbol redefinition: %s",tkName->text);
                var=newSymbol(tkName->text,SK_VAR);
                var->type=t;
                var->owner=owner;
                addSymbolToDomain(symTable,var);
                if(owner){
                    switch(owner->kind){
                        case SK_FN:
                            var->varIdx=symbolsLen(owner->fn.locals);
                            addSymbolToList(&owner->fn.locals,dupSymbol(var));
                            break;
                        case SK_STRUCT:
                            var->varIdx=typeSize(&owner->type);
                            addSymbolToList(&owner->structMembers,dupSymbol(var));
                            break;
                        }
                }else{
                    var->varIdx=allocInGlobalMemory(typeSize(&t));
                }
                return true;
            }
            else tkerr(iTk, "Need: ;");
        }
        else tkerr(iTk, "Need identifier after type declaration");
    }
    iTk = start;
    return false;
}

bool typeBase(Type* t){
    Token *start = iTk;
    t->n=-1;
    if(consume(INT)){
        t->tb=TB_INT;
        return true;
    }
    if(consume(DOUBLE)){
        t->tb=TB_DOUBLE;
        return true;
    }
    if(consume(CHAR)){
        t->tb=TB_CHAR;
        return true;
    }
    if(consume(STRUCT)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            t->tb=TB_STRUCT;
            t->s=findSymbol(tkName->text);
            if(!t->s)tkerr(iTk,"structura nedefinita: %s",tkName->text);
            return true;
        }
        else tkerr(iTk, "Need struct identifier");
    }

    iTk = start;

    return false;
}

bool arrayDecl(Type* t){
    Token *start = iTk;
    if(consume(LBRACKET)){
        if(consume(CT_INT)){
            Token* tkSize = consumedTk;
            t->n = tkSize->i;
        }
        if(consume(RBRACKET)){
            return true;
        }
        else tkerr(iTk, "Need: ]");
    }
    iTk = start;
    return false;
}

bool fnDef(){
    Token *start = iTk;
    Type t;
    if(typeBase(&t)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            if(consume(LPAR)){
                Symbol *fn=findSymbolInDomain(symTable,tkName->text);
                if(fn)
                    tkerr(iTk,"symbol redefinition: %s",tkName->text);
                fn=newSymbol(tkName->text,SK_FN);
                fn->type=t;
                addSymbolToDomain(symTable,fn);
                owner=fn;
                pushDomain();

                if(fnParam()){
                    for(;;){
                        if(consume(COMMA)){
                            if(fnParam()){}
                            else tkerr(iTk, "Need parameter after ,");
                        }
                        else break;
                    }
                }
                if(consume(RPAR)){
                    if(stmCompound(false)){
                        dropDomain();
                        owner=NULL;
                        return true;
                    }
                    else tkerr(iTk, "Need function implementation!");
                }
                else tkerr(iTk, "Need: )");
            }
        }
        else tkerr(iTk, "Need function identifier");
    }
    if(consume(VOID)){
        t.tb = TB_VOID;
        if(consume(ID)){
            Token* tkName = consumedTk;
            if(consume(LPAR)){
                Symbol *fn=findSymbolInDomain(symTable,tkName->text);
                if(fn)
                    tkerr(iTk,"symbol redefinition: %s",tkName->text);
                fn=newSymbol(tkName->text,SK_FN);
                fn->type=t;
                addSymbolToDomain(symTable,fn);
                owner=fn;
                pushDomain();

                if(fnParam()){
                    for(;;){
                        if(consume(COMMA)){
                            if(fnParam()){}
                            else tkerr(iTk, " parameter missing after ',' !");
                        }
                        else break;
                    }
                }
                if(consume(RPAR)){
                    if(stmCompound(false)){
                        dropDomain();
                        owner=NULL;
                        return true;
                    }
                    else tkerr(iTk, " function implementation missing!");
                }
                else tkerr(iTk, " ')' missing!");
            }
        }
        else tkerr(iTk, " function identifier missing!");
    }
    iTk = start;
    return false;
}

bool fnParam(){
    Token *start = iTk;
    Type t;
    if(typeBase(&t)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            if(arrayDecl(&t)){
                t.n = 0;
            }

            Symbol *param=findSymbolInDomain(symTable,tkName->text);
            if(param)
                tkerr(iTk,"symbol redefinition: %s",tkName->text);
            param=newSymbol(tkName->text,SK_PARAM);
            param->type=t;
            param->paramIdx=symbolsLen(owner->fn.params);
            // parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
            addSymbolToDomain(symTable,param);
            addSymbolToList(&owner->fn.params,dupSymbol(param));

            return true;
        }
        else tkerr(iTk, "Need type identifier");
    }

    iTk = start;

    return false;
}

int returnFound=0;
bool stm(){
    Token *start = iTk;
    Ret rInit, rCond, rStep, rExpr;
    if(stmCompound(true)){
        return true;
    }
    iTk = start;
    if(consume(IF)){
        if(consume(LPAR)){
            if(expr(&rCond)){
                if(!canBeScalar(&rCond))
                    tkerr(iTk,"the if condition must be a scalar value");

                if(consume(RPAR)){
                    if(stm()){
                        if(consume(ELSE)){
                            if(stm()){
                                return true;
                            }
                            else tkerr(iTk, "Need: else brach");
                        }
                        return true;
                    }
                    else tkerr(iTk, "Need: if branch");
                }
                else tkerr(iTk, "Need: )");
            }
            else tkerr(iTk, "Need if condition");
        }
        else tkerr(iTk, "Need: (");
    }
    iTk = start;
    if(consume(WHILE)){
        if(consume(LPAR)){
            if(expr(&rCond)){
                if(!canBeScalar(&rCond))
                    tkerr(iTk,"the while condition must be a scalar value");

                if(consume(RPAR)){
                    if(stm()){
                        return true;
                    }
                    else tkerr(iTk, "Need: while");
                }
                else tkerr(iTk, "Need: )");
            }
            else tkerr(iTk, "Need: condition");
        }
        else tkerr(iTk, "Need: (");
    }
    iTk = start;
    if(consume(FOR)){
        if(consume(LPAR)){
            if(expr(&rInit)){}
            if(consume(SEMICOLON)){
                if(expr(&rCond)){
                    if(!canBeScalar(&rCond))
                        tkerr(iTk,"the for condition must be a scalar value");
                }
                if(consume(SEMICOLON)){
                    if(expr(&rStep)){}
                    if(consume(RPAR)){
                        if(stm()){
                            return true;
                        }
                        else tkerr(iTk, "Need: for condition");
                    }
                    else tkerr(iTk, "Need: )");
                }
                else tkerr(iTk, "Need: ;");
            }
            else tkerr(iTk, "Need: ;");
        }
        else tkerr(iTk, "Need: (");
    }
    iTk = start;
    if(consume(BREAK)){
        if(consume(SEMICOLON)){
            return true;
        }
        else tkerr(iTk, "Need: ;");
    }
    iTk = start;
    if(consume(RETURN)){
        returnFound=1;
        if(expr(&rExpr)){
            if(owner->type.tb==TB_VOID)
                tkerr(iTk,"a void function cannot return a value");
            if(!canBeScalar(&rExpr))
                tkerr(iTk,"the return value must be a scalar value");
            if(!convTo(&rExpr.type,&owner->type))
                tkerr(iTk,"cannot convert the return expression type to the function return type");
        }
        if(consume(SEMICOLON)){
            return true;
        }
        else tkerr(iTk, "Need: ;");
    }
    //else if(owner->type.tb!=TB_VOID)
                //tkerr(iTk,"a non-void function must return a value");

    iTk = start;

    if(expr(&rExpr)){}
    if(consume(SEMICOLON)){
        return true;
    }
    iTk = start;
    return false;
}

bool stmCompound(bool newDomain){
    Token *start = iTk;
    if(consume(LACC)){
        if(newDomain)pushDomain();
        for(;;){
            if(varDef()){}
            else if(stm()){}
            else break;
        }
        if(consume(RACC)){
            if(owner->type.tb!=TB_VOID && returnFound==0){
                tkerr(iTk,"a non-void function must return a value");
            }
            else if(returnFound==1) returnFound=0;

            if(newDomain)dropDomain();
            return true;
        }
        else tkerr(iTk, "Need: }");
    }
    iTk = start;
    return false;
}

bool expr(Ret *r){
    Token *start = iTk;
    if(exprAssign(r)){
        return true;
    }
    iTk = start;
    return false;
}

bool exprAssign(Ret *r){
    Token *start = iTk;
    Ret rDst;
    if(exprUnary(&rDst)){
        if(consume(ASSIGN)){
            if(exprAssign(r)){
                if(!rDst.lval)
                    tkerr(iTk,"the assign destination must be a left-value");
                if(rDst.ct)
                    tkerr(iTk,"the assign destination cannot be constant");
                if(!canBeScalar(&rDst))
                    tkerr(iTk,"the assign destination must be scalar");
                if(!canBeScalar(r))
                    tkerr(iTk,"the assign source must be scalar");
                if(!convTo(&r->type,&rDst.type))
                    tkerr(iTk,"the assign source cannot be converted to destination");
                r->lval=false;
                r->ct=true;

                return true;
            }
            else tkerr(iTk, "Need: right expression term");
        }
    }
    iTk = start;
    if(exprOr(r)){
        return true;
    }
    iTk = start;
    return false;
}

bool exprOr(Ret *r){
    Token *start = iTk;
    if(exprAnd(r)){
        if(exprOrPrim(r)){
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprOrPrim(Ret *r){
    Token *start = iTk;
    if(consume(OR)){
        Ret right;
        if(exprAnd(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk,"invalid operand type for ||");
            *r=(Ret){{TB_INT,NULL,-1},false,true};

            if(exprOrPrim(r)){
                return true;
            }
        }
        else tkerr(iTk, "Need expression after ||");
    }
    iTk = start;
    return true;
}

bool exprAnd(Ret *r){
    Token *start = iTk;
    if(exprEq(r)){
        if(exprAndPrim(r)){
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprAndPrim(Ret *r){
    Token *start = iTk;
    if(consume(AND)){
        Ret right;
        if(exprEq(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk,"Invalid operand type for &&");
            *r=(Ret){{TB_INT,NULL,-1},false,true};

            if(exprAndPrim(r)){
                return true;
            }
        }
        else tkerr(iTk, "Need expression after &&");
    }
    iTk = start;
    return true;
}

bool exprEq(Ret *r){
    Token *start = iTk;
    if(exprRel(r)){
        if(exprEqPrim(r)){
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprEqPrim(Ret *r){
    Token *start = iTk;
    char sign[20];
    strcpy(sign, map[start -> code]);
    if(consume(EQUAL) || consume(NOTEQ)){
        Ret right;
        if(exprRel(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk,"invalid operand type for %s", sign);
            *r=(Ret){{TB_INT,NULL,-1},false,true};

            if(exprEqPrim(r)){
                return true;
            }
        }
        else tkerr(iTk, "Invalid or missing expression after %s sign", sign);
    }
    iTk = start;
    return true;
}

bool exprRel(Ret *r){
    Token *start = iTk;
    if(exprAdd(r)){
        if(exprRelPrim(r)){
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprRelPrim(Ret *r){
    Token *start = iTk;
    char sign[20];
    strcpy(sign, map[start -> code]);
    if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)){
        Ret right;
        if(exprAdd(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk,"invalid operand type for %s", sign);
            *r=(Ret){{TB_INT,NULL,-1},false,true};

            if(exprRelPrim(r)){
                return true;
            }
        }
        else tkerr(iTk, "Invalid or missing expression after %s sign", sign);
    }
    iTk = start;
    return true;
}

bool exprAdd(Ret *r){
    Token *start = iTk;
    if(exprMul(r)){
        if(exprAddPrim(r)){
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprAddPrim(Ret *r){
    Token *start = iTk;
    char sign[20];
    strcpy(sign, map[start -> code]);
    if(consume(ADD) || consume(SUB)){
        Ret right;
        if(exprMul(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk,"Invalid operand type for %s", sign);
            *r=(Ret){{TB_INT,NULL,-1},false,true};

            if(exprAddPrim(r)){
                return true;
            }
        }
        else tkerr(iTk, "Invalid or missing expression after %s", sign);
    }
    iTk = start;
    return true;
}

bool exprMul(Ret *r){
    Token *start = iTk;
    if(exprCast(r)){
        if(exprMulPrim(r)){
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprMulPrim(Ret *r){
    Token *start = iTk;
    char sign[20];
    strcpy(sign, map[start -> code]);
    if(consume(MUL) || consume(DIV)){
        Ret right;
        if(exprCast(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst))
                tkerr(iTk,"invalid operand type for %s sign!", sign);
            *r=(Ret){{TB_INT,NULL,-1},false,true};

            if(exprMulPrim(r)){
                return true;
            }
        }
        else tkerr(iTk, "Invalid or missing expression after %s", sign);
    }
    iTk = start;
    return true;
}

bool exprCast(Ret *r){
    Token *start = iTk;
    Type t;
    if(consume(LPAR)){
        Type t;
        Ret op;
        if(typeBase(&t)){
            if(arrayDecl(&t)){}
            if(consume(RPAR)){
                if(exprCast(&op)){
                    if(t.tb==TB_STRUCT)
                        tkerr(iTk,"cannot convert to a struct type");
                    if(op.type.tb==TB_STRUCT)
                        tkerr(iTk,"cannot convert a struct");
                    if(op.type.n>=0&&t.n<0)
                        tkerr(iTk,"an array can be converted only to another array");
                    if(op.type.n<0&&t.n>=0)
                        tkerr(iTk,"a scalar can be converted only to another scalar");
                    *r=(Ret){t,false,true};
                    return true;
                }
            }
            else tkerr(iTk, "Need: )");
        }
    }
    iTk = start;
    if(exprUnary(r)){
        return true;
    }
    iTk = start;
    return false;
}

bool exprUnary(Ret *r){
    Token *start = iTk;
    char c;
    if(start->code == NOT)
      c = '!';
    else
      c='-';
    if(consume(SUB) || consume(NOT)){
        if(exprUnary(r)){
            if(!canBeScalar(r))
                tkerr(iTk,"unary - must have a scalar operand");
            r->lval=false;
            r->ct=true;

            return true;
        }
        else tkerr(iTk, " expression missing after %c!", c);
    }
    iTk = start;
    if(exprPostfix(r)){
        return true;
    }
    iTk = start;
    return false;
}

bool exprPostfix(Ret *r){
    Token *start = iTk;
    if(exprPrimary(r)){
        if(exprPostfixPrim(r)){
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprPostfixPrim(Ret *r){
    Token *start = iTk;
    if(consume(LBRACKET)){
        Ret idx;
        if(expr(&idx)){
            if(consume(RBRACKET)){
                if(r->type.n<0)
                    tkerr(iTk,"only an array can be indexed");
                Type tInt={TB_INT,NULL,-1};
                if(!convTo(&idx.type,&tInt))
                    tkerr(iTk,"the index is not convertible to int");
                r->type.n=-1;
                r->lval=true;
                r->ct=false;

                if(exprPostfixPrim(r)){
                    return true;
                }
            }
             else tkerr(iTk, "Need: ]");
        }
        else tkerr(iTk, "Need expression between []");
    }
    iTk = start;
    if(consume(DOT)){
        if(consume(ID)){
            Token* tkName = consumedTk;
            if(r->type.tb!=TB_STRUCT)
                tkerr(iTk,"a field can only be selected from a struct");
            Symbol *s=findSymbolInList(r->type.s->structMembers,tkName->text);
            if(!s)
                tkerr(iTk,"the structure %s does not have a field  %s",r->type.s->name,tkName->text);
            *r=(Ret){s->type,true,s->type.n>=0};

            if(exprPostfixPrim(r)){
                return true;
            }
        }
        else tkerr(iTk, "Need identifier after .");
    }
    iTk = start;
    return true;
}

bool exprPrimary(Ret *r){
    Token *start = iTk;
    if(consume(ID)){
        Token* tkName = consumedTk;
        Symbol *s=findSymbol(tkName->text);
        if(!s)
            tkerr(iTk,"undefined id: %s",tkName->text);

        if(consume(LPAR)){
            if(s->kind!=SK_FN)
                tkerr(iTk,"only a function can be called");
            Ret rArg;
            Symbol *param=s->fn.params;

            if(expr(&rArg)){
                if(!param)
                    tkerr(iTk,"too many arguments in function call");
                if(!convTo(&rArg.type,&param->type))
                    tkerr(iTk,"in call, cannot convert the argument type to the parameter type");
                param=param->next;

                while(consume(COMMA)){
                    if(expr(&rArg)){
                        if(!param)
                            tkerr(iTk,"too many arguments in function call");
                        if(!convTo(&rArg.type,&param->type))
                            tkerr(iTk,"in call, cannot convert the argument type to the parameter type");
                        param=param->next;
                    }
                    else tkerr(iTk, "Need expression after ,");
                }
            }
            if(consume(RPAR)){
                if(param)
                    tkerr(iTk,"too few arguments in function call");
                *r=(Ret){s->type,false,true};

                return true;
            }
            else tkerr(iTk, "Need: )");
        }
        else {
            if(s->kind==SK_FN)
                tkerr(iTk,"a function can only be called");
            *r=(Ret){s->type,true,s->type.n>=0};
        }
        return true;
    }
    iTk = start;
    if(consume(CT_INT)){
        *r=(Ret){{TB_INT,NULL,-1},false,true};
        return true;
    }
    if(consume(CT_REAL)){
        *r=(Ret){{TB_DOUBLE,NULL,-1},false,true};
        return true;
    }
    if(consume(CT_CHAR)){
        *r=(Ret){{TB_CHAR,NULL,-1},false,true};
        return true;
    }
    if(consume(CT_STRING)){
        *r=(Ret){{TB_CHAR,NULL,0},false,true};
        return true;
    }
    if(consume(LPAR)){
        if(expr(r)){
            if(consume(RPAR)){
                return true;
            }
            else tkerr(iTk, "Need: )");
        }
        else  tkerr(iTk, "Need expression after (");
    }
    iTk = start;
    return false;
}

int main()
{
  FILE *fis;
  fis = fopen("testAnalizatorTipuri.c", "rb");

  if (fis == NULL){
    printf("nu s-a putut deschide fisierul");
    return -1;
  }

  int n = fread(bufin, 1, 30000, fis);
  bufin[n] = '\0';
  fclose(fis);
  pCrtCh = bufin; // initializarea pch pe prima pozitie din buffin

  while(getNextToken() != END){}

  //showAtoms();

  iTk = tokens;
  bool start = unit();
  // printf("%d\n", start);
  //showAtoms();


  free(iTk);
  free(consumedTk);
  free(tokens);
  free(lastToken);
  return 0;
}
