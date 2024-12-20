
package rs.ac.bg.etf.pp1;

import java_cup.runtime.Symbol;

%%

%{

	boolean errorDetected = false;
	
	private void setErrorDetected(){
		errorDetected = true;
	}

	// ukljucivanje informacije o poziciji tokena
	private Symbol new_symbol(int type) {
		return new Symbol(type, yyline+1, yycolumn);
	}
	
	// ukljucivanje informacije o poziciji tokena
	private Symbol new_symbol(int type, Object value) {
		return new Symbol(type, yyline+1, yycolumn, value);
	}

%}

%cup
%line
%column

%xstate COMMENT

%eofval{
	return new_symbol(sym.EOF);
%eofval}

%%

" " 							{ }
"\b" 							{ }
"\t" 							{ }
"\r\n" 							{ }
"\f" 							{ }

// Keywords
"program"   					{ return new_symbol(sym.PROG, yytext());}
//"break"   						{ return new_symbol(sym.BREAK, yytext());}
//"class"   						{ return new_symbol(sym.CLASS, yytext());}
//"if"   							{ return new_symbol(sym.IF, yytext());}
//"else"   						{ return new_symbol(sym.ELSE, yytext());}
"const"  						{ return new_symbol(sym.CONST, yytext());}
"new"   						{ return new_symbol(sym.NEW, yytext());}
"print" 						{ return new_symbol(sym.PRINT, yytext()); }
"return" 						{ return new_symbol(sym.RETURN, yytext()); }
"void" 							{ return new_symbol(sym.VOID, yytext()); }
"read"   						{ return new_symbol(sym.READ, yytext());}
//"extends"   					{ return new_symbol(sym.EXTENDS, yytext());}
//"continue"   					{ return new_symbol(sym.CONTINUE, yytext());}
//"for"   						{ return new_symbol(sym.FOR, yytext());}
//"static"   						{ return new_symbol(sym.STATIC, yytext());}
//"namespace"   					{ return new_symbol(sym.NAMESPACE, yytext());}
"range"							{ return new_symbol(sym.RANGE, yytext()); }
//"in"							{ return new_symbol(sym.IN, yytext()); }

// Token types
[0-9]+  						{ return new_symbol(sym.NUMBER, new Integer (yytext())); }
"'"."'"							{ return new_symbol(sym.CHAR, new Character (yytext().charAt(1))); }	
("true"|"false")				{ return new_symbol(sym.BOOL, yytext().equals("true") ? 1 : 0); }
([a-z]|[A-Z])[a-z|A-Z|0-9|_]* 	{ return new_symbol(sym.IDENT, yytext()); }				

//Operators
//"==" 							{ return new_symbol(sym.EQUALS, yytext()); }
//"!=" 							{ return new_symbol(sym.NOTEQUALS, yytext()); }
//">=" 							{ return new_symbol(sym.GREATEREQ, yytext()); }
//">" 							{ return new_symbol(sym.GREATER, yytext()); }
//"<=" 							{ return new_symbol(sym.LESSEQ, yytext()); }
//"<" 							{ return new_symbol(sym.LESS, yytext()); }
"++" 							{ return new_symbol(sym.INC, yytext()); }
"--" 							{ return new_symbol(sym.DEC, yytext()); }
"+" 							{ return new_symbol(sym.PLUS, yytext()); }
"-" 							{ return new_symbol(sym.MINUS, yytext()); }
"*" 							{ return new_symbol(sym.MUL, yytext()); }
"/" 							{ return new_symbol(sym.DIV, yytext()); }
"%" 							{ return new_symbol(sym.MODULE, yytext()); }
//"&&" 							{ return new_symbol(sym.LOGAND, yytext()); }
//"||" 							{ return new_symbol(sym.LOGOR, yytext()); }
"=" 							{ return new_symbol(sym.ASSIGN, yytext()); }
";" 							{ return new_symbol(sym.SEMI, yytext()); }
":" 							{ return new_symbol(sym.COLON, yytext()); }
"," 							{ return new_symbol(sym.COMMA, yytext()); }
//"." 							{ return new_symbol(sym.DOT, yytext()); }
"(" 							{ return new_symbol(sym.LPAREN, yytext()); }
")" 							{ return new_symbol(sym.RPAREN, yytext()); }
"{" 							{ return new_symbol(sym.LBRACE, yytext()); }
"}"								{ return new_symbol(sym.RBRACE, yytext()); }
"[" 							{ return new_symbol(sym.LBRACKET, yytext()); }
"]"								{ return new_symbol(sym.RBRACKET, yytext()); }
//"=>"							{ return new_symbol(sym.ARROW, yytext()); }

// Comments
"//" 							{ yybegin(COMMENT);}
<COMMENT> . 					{ yybegin(COMMENT);}
<COMMENT> "\r\n" 				{ yybegin(YYINITIAL); }


. { setErrorDetected(); System.err.println("Lexical error ("+yytext()+") in line: "+(yyline+1)); }










