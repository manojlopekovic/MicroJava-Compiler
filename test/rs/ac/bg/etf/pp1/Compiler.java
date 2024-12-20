package rs.ac.bg.etf.pp1;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Scanner;

import org.apache.log4j.Logger;
import org.apache.log4j.xml.DOMConfigurator;

import java_cup.runtime.Symbol;
import rs.ac.bg.etf.pp1.ast.Program;
import rs.ac.bg.etf.pp1.util.Log4JUtils;
import rs.etf.pp1.mj.runtime.Code;
import rs.etf.pp1.symboltable.Tab;
import rs.etf.pp1.symboltable.concepts.Obj;
import rs.etf.pp1.symboltable.concepts.Struct;

public class Compiler {


	static {
		DOMConfigurator.configure(Log4JUtils.instance().findLoggerConfigFile());
		Log4JUtils.instance().prepareLogFile(Logger.getRootLogger());
	}
	
	public static void main(String[] args) throws Exception {
		
		Logger log = Logger.getLogger(Compiler.class);
		
		Reader br = null;
		try {
			File sourceCode = new File("test/program.mj");
			log.info("Compiling source file: " + sourceCode.getAbsolutePath());
			
			br = new BufferedReader(new FileReader(sourceCode));
			Yylex lexer = new Yylex(br);
			
			MJParser p = new MJParser(lexer);
	        Symbol s = p.parse();  // Start of parsing
	        
	        if(lexer.errorDetected) {
				log.info(" LEXICAL ERROR ");
				return;
			}
	        
	        Program prog = (Program)(s.value); 
			// writing AST
			log.info("======================================================================");
			log.info(prog.toString(""));
			log.info("======================================================================");

	     
			if(p.errorDetected) {
				log.info(" ERROR IN PARSING ");
				return;
			}
			log.info(" SUCCESFULL PARSING ");
			log.info("======================================================================\n");
			
			
//			Init of Symbol Table
			Tab.init();
//			Inserting bool type
			Struct boolType = new Struct(Struct.Bool);
//			Ime strukturnog cvora se ne ispisuje
			Obj boolObj = Tab.insert(Obj.Type, "bool", boolType);
			boolObj.setAdr(-1);
			boolObj.setLevel(-1);
			
//			Semantic Analysis
			SemanticAnalyzer sAnalyzer = new SemanticAnalyzer();
//			Prog is only AST tree
			prog.traverseBottomUp(sAnalyzer);
			
//			Writing Symbol table
			Tab.dump();
			log.info("======================================================================");
			
			if(!sAnalyzer.succesfull_pass()) {
				log.info(" ERROR IN SEMANTIC ANALYSIS ");
				return;
			}
			log.info(" SUCCESFULL SEMANTIC ANALYSIS ");
			log.info("======================================================================\n\n");
			
//			Generating code
//			Scanner scanner = new Scanner(System.in);
//			System.out.println("Enter name of bytecode file:");
//			String fileNameString = scanner.nextLine();
//			String filePathString = "./" + fileNameString + ".obj";
			File file = new File("./prog.obj");
			if(file.exists()) 
				file.delete();
			
			CodeGen codeGen = new CodeGen();
			prog.traverseBottomUp(codeGen);
			Code.dataSize = sAnalyzer.getNVars();
			Code.mainPc = codeGen.getStartAddr();
			Code.write(new FileOutputStream(file));

			log.info("Generating file: prog.obj");
			log.info("======================================================================");
			log.info(" SUCCESFULL CODE GENERATION ");
			log.info("======================================================================");
//	        scanner.close();
			
		} 
		finally {
			if (br != null) try { br.close(); } catch (IOException e1) { log.error(e1.getMessage(), e1); }
		}

	}
}
