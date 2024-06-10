package rs.ac.bg.etf.pp1;

import org.apache.log4j.Logger;

import rs.ac.bg.etf.pp1.ast.*;
import rs.etf.pp1.symboltable.Tab;
import rs.etf.pp1.symboltable.concepts.Obj;
import rs.etf.pp1.symboltable.concepts.Struct;

// Visitor adaptor is apstract class that implements Visit interface
// Visit interface has methods visit, for each node in tree, that should be overwritten
public class SemanticAnalyzer extends VisitorAdaptor {

	private Obj myProg;
	private Struct currentType;
	private int constant;
	private Struct constantType;
	private Struct boolType = Tab.find("bool").getType();
	private boolean ismain = false;
	private Obj currentMethod;

//	Semantic pass code
//****************************************************************************************************************
	
//	Always for concrete classes, not abstract
	@Override
	public void visit(Program program) {
//		Closing scope on end
//		If we do find, it will take [name] from current scope
		Tab.chainLocalSymbols(myProg);
		Tab.closeScope();
		myProg = null;
		if(!ismain)
			error_report("No main method", program);
	}
	
	@Override
	public void visit(ProgramName programName) {
//		Creating program scope
		myProg = Tab.insert(Obj.Prog, programName.getPName(), Tab.noType);
		Tab.openScope();
	}
	
//	Var declarations
	@Override
	public void visit(VarDeclarationVar varDeclarationVar) {
		Obj varObj = null;
		if(currentMethod == null) {
			varObj = Tab.find(varDeclarationVar.getVName());
		} else {
//			Finds in all scopes, not just open one
			varObj = Tab.currentScope().findSymbol(varDeclarationVar.getVName());
		}
		if(varObj != null)
			if(varObj != Tab.noObj) {
				error_report("Double declaration of variable: " + varDeclarationVar.getVName(), varDeclarationVar);
				return;
			} 
		Tab.insert(Obj.Var, varDeclarationVar.getVName(), currentType);
	}
	
	@Override
	public void visit(VarDeclarationArray varDeclarationArray) {
		Obj varObj = null;
		if(currentMethod == null) {
			varObj = Tab.find(varDeclarationArray.getArrName());
		} else {
			varObj = Tab.currentScope().findSymbol(varDeclarationArray.getArrName());
		}
		if(varObj != null)
			if(varObj != Tab.noObj) {
				error_report("Double declaration of array: " + varDeclarationArray.getArrName(), varDeclarationArray);
				return;
			} 
		Tab.insert(Obj.Var, varDeclarationArray.getArrName(), new Struct(Struct.Array, currentType));
	}
	
//	Method declarations
	public void visit(MethodName methodName) {
//		For A, we don't have to check if multiple methods
		if(methodName.getMName().equalsIgnoreCase("main")) 
			ismain  = true;			
		currentMethod = Tab.insert(Obj.Meth, methodName.getMName(), Tab.noType);
		Tab.openScope();
	}
	
	@Override
	public void visit(MethodDecl methodDecl) {
		Tab.chainLocalSymbols(currentMethod);
		Tab.closeScope();
		currentMethod = null;
	}
	
	
//	Constant declaration

	@Override
	public void visit(Type type) {
//		If class[When doing C], name must be declared before
		Obj typeObj = Tab.find(type.getTName());
		if(typeObj == Tab.noObj) {
			error_report("Type is unknown: " + type.getTName(), type);
			currentType = Tab.noType;
			return;
		}else if(typeObj.getKind() != Obj.Type) {
			error_report("Unadequate type: " + type.getTName(), type);
			currentType = Tab.noType;
			return;
		}
		currentType = typeObj.getType();
	}
	
	@Override
	public void visit(TypeConstNum typeConstNum) {
		constant = typeConstNum.getConstVal();
		constantType = Tab.intType;
	}
	
	@Override
	public void visit(TypeConstChar typeConstChar) {
		constant = typeConstChar.getConstVal();
		constantType = Tab.charType;
	}
	
	@Override
	public void visit(TypeConstBool typeConstBool) {
		constant = typeConstBool.getConstVal();
//		Tab.find(bool)
		constantType = boolType;
	}
	
	@Override
	public void visit(ConstDeclAssign constDeclAssign) {
//		Is constant there
		if(Tab.find(constDeclAssign.getVName()) != Tab.noObj) {
			error_report("Double definition of constant: " + constDeclAssign.getVName(), constDeclAssign);
			return;
		} else if(!constantType.assignableTo(currentType)) {
			error_report("Trying to assign: " + constantType.getKind() + " to: " + currentType.getKind(), constDeclAssign);
			return;
		}
//		Equals, compatible with, assignable to with struct types comparison
//		We can go through parents, but we would need to do it in while loop, because we don't know where Type is
		Obj conObj = Tab.insert(Obj.Con, constDeclAssign.getVName(), currentType);
//		We must write value to adr field for constants
		conObj.setAdr(constant);
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
