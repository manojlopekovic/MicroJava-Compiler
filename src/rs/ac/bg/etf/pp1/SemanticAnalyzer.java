package rs.ac.bg.etf.pp1;

import org.apache.log4j.Logger;

import rs.ac.bg.etf.pp1.ast.*;
import rs.etf.pp1.symboltable.Tab;
import rs.etf.pp1.symboltable.concepts.Obj;

// Visitor adaptor is apstract class that implements Visit interface
// Visit interface has methods visit, for each node in tree, that should be overwritten
public class SemanticAnalyzer extends VisitorAdaptor {

//	Semantic pass code
//****************************************************************************************************************
	
//	Always for concrete classes, not abstract
	@Override
	public void visit(Program program) {
		
	}
	
	@Override
	public void visit(ProgramName programName) {
//		Creating program scope
		Tab.insert(Obj.Prog, programName.getPName(), Tab.noType);
		Tab.openScope();
	}
	
	
	
//****************************************************************************************************************	
//	Semantic reporting code
//****************************************************************************************************************
	private boolean errDetected = false;
	Logger log = Logger.getLogger(getClass());
	
//	Write error
	public void error_report(String msg, SyntaxNode info) {
		errDetected = true;
		if(info != null)
			msg = msg.concat(" on line: ".concat(String.valueOf(info.getLine())));
		log.error(msg);		
	}
	
//	Write info
	public void info_report(String msg, SyntaxNode info) {
		if(info != null)
			msg = msg.concat(" on line: ".concat(String.valueOf(info.getLine())));
		log.info(msg);		
	}
	
	public boolean succesfull_pass() {
		return !errDetected;
	}
//****************************************************************************************************************
}
