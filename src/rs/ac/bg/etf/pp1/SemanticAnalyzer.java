package rs.ac.bg.etf.pp1;

import org.apache.log4j.Logger;

import rs.ac.bg.etf.pp1.ast.*;
import rs.etf.pp1.symboltable.Tab;
import rs.etf.pp1.symboltable.concepts.Obj;
import rs.etf.pp1.symboltable.concepts.Struct;

// Visitor adaptor is abstract class that implements Visit interface
// Visit interface has methods visit, for each node in tree, that should be overwritten
public class SemanticAnalyzer extends VisitorAdaptor {
	
//	Todo : Do report info from specification of project

	private Obj myProg;
	private Struct currentType;
	private int constant;
	private Struct constantType;
	private Struct boolType = Tab.find("bool").getType();
	private Struct intType = Tab.intType;
	private Struct charType = Tab.charType;
	private Struct noType = Tab.noType;
	private boolean ismain = false;
	private Obj currentMethod;
	private int nVars;


//****************************************************************************************************************
//	Semantic pass code, context conditions
//****************************************************************************************************************

//	Statement
	
	@Override
	public void visit(StmtRead stmtRead) {
//		Check if variable, element of an array [Field inside class for C]
		if(stmtRead.getDesignator().obj.getKind() != Obj.Var && stmtRead.getDesignator().obj.getKind() != Obj.Elem) {
			error_report("Trying to read: " + stmtRead.getDesignator().obj.getName() + " which is of unreadable type", stmtRead);
			return;
		}
		Struct typeStruct = stmtRead.getDesignator().obj.getType();
		if(!typeStruct.equals(intType) && !typeStruct.equals(charType)  && !typeStruct.equals(boolType)) {
			error_report("Trying to read: " + stmtRead.getDesignator().obj.getName() + " which is not integer, char or bool type", stmtRead);
			return;
		}
	}
	
	@Override
	public void visit(StmtPrintExpr stmtPrint) {
//		Check if variable, element of an array [Field inside class for C]
		Struct typeStruct = stmtPrint.getExpr().struct;
		if(typeStruct.getKind() != Struct.Array && !typeStruct.equals(intType) && !typeStruct.equals(boolType)  && !typeStruct.equals(charType)) {
			error_report("Trying to print: " + typeStruct.toString() + " which is not array, integer, char or bool type", stmtPrint);
			return;
		}
	}
	
	@Override
	public void visit(StmtPrintNum stmtPrint) {
//		Check if variable, element of an array [Field inside class for C]
		Struct typeStruct = stmtPrint.getExpr().struct;
		if(typeStruct.getKind() != Struct.Array && !typeStruct.equals(intType) && !typeStruct.equals(boolType)  && !typeStruct.equals(charType)) {
			error_report("Trying to print: " + typeStruct.toString() + " which is not array, integer, char or bool type, with params", stmtPrint);
			return;
		}
	}
	
	
//	Designator statement
	@Override
	public void visit(DStmtDesAExpr dStmtDesAExpr) {
//		Check if variable, element of an array [Field inside class for C]
		if(dStmtDesAExpr.getDesignator().obj.getKind() != Obj.Var && dStmtDesAExpr.getDesignator().obj.getKind() != Obj.Elem) {
			error_report("Trying to assign to: " + dStmtDesAExpr.getDesignator().obj.getName() + " which is of unassignable type", dStmtDesAExpr);
			return;
		}
		if(!dStmtDesAExpr.getExpr().struct.assignableTo(dStmtDesAExpr.getDesignator().obj.getType())) {
			error_report("Trying to assign variable type: " + dStmtDesAExpr.getExpr().struct.getKind() + " to: " + dStmtDesAExpr.getDesignator().obj.getName(), dStmtDesAExpr);
			return;
		}
	}

	@Override
	public void visit(DStmtDesInc dStmtDesInc) {
//		Check if variable, element of an array [Field inside class for C]
		if(dStmtDesInc.getDesignator().obj.getKind() != Obj.Var && dStmtDesInc.getDesignator().obj.getKind() != Obj.Elem) {
			error_report("Trying to increment: " + dStmtDesInc.getDesignator().obj.getName() + " which is of unassignable type", dStmtDesInc);
			return;
		}
		if(!dStmtDesInc.getDesignator().obj.getType().equals(intType)) {
			error_report("Operand for incrementing: " + dStmtDesInc.getDesignator().obj.getName() +" is not integer type", dStmtDesInc);
			return;
		}
	}
	
	@Override
	public void visit(DStmtDesDec dStmtDesDec) {
//		Check if variable, element of an array [Field inside class for C]
		if(dStmtDesDec.getDesignator().obj.getKind() != Obj.Var && dStmtDesDec.getDesignator().obj.getKind() != Obj.Elem) {
			error_report("Trying to decrement: " + dStmtDesDec.getDesignator().obj.getName() + " which is of unassignable type", dStmtDesDec);
			return;
		}
		if(!dStmtDesDec.getDesignator().obj.getType().equals(intType)) {
			error_report("Operand for decrementing: " + dStmtDesDec.getDesignator().obj.getName() +" is not integer type", dStmtDesDec);
			return;
		}
	}
	
	
	
//	Expr, Term
	
	@Override
	public void visit(MulFactorFactor mulFactorFactor) {
		if(mulFactorFactor.getMulFactor() instanceof MulFactorFactor) {
			if(!mulFactorFactor.getMulFactor().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + mulFactorFactor.getMulFactor().struct + " in multiplication operation", mulFactorFactor);
				mulFactorFactor.struct = noType;
				return;
			}
		}
		if(!mulFactorFactor.getFactor().struct.equals(intType)) {
			error_report("Trying to assign non-int value: " + mulFactorFactor.getFactor().struct + " in multiplication operation", mulFactorFactor);
			mulFactorFactor.struct = noType;
			return;
		}
		mulFactorFactor.struct = intType;
	}
	
	@Override
	public void visit(Term term) {
		if(term.getMulFactor() instanceof MulFactorFactor) {
			if(!term.getMulFactor().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + term.getMulFactor().struct + "  in term", term);
				term.struct = noType;
				return;
			} 
			if(!term.getFactor().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + term.getFactor().struct + "  in term", term);
				term.struct = noType;
				return;
			}
		}
		term.struct = term.getFactor().struct;
	}
	
	@Override
	public void visit(AddTermTerm addTermTerm) {
		if(addTermTerm.getAddTerm() instanceof AddTermTerm) {
			if(!addTermTerm.getAddTerm().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + addTermTerm.getAddTerm().struct + " in addition operation", addTermTerm);
				addTermTerm.struct = noType;
				return;
			}
		}
		if(!addTermTerm.getTerm().struct.equals(intType)) {
			error_report("Trying to assign non-int value: " + addTermTerm.getTerm().struct + " in addition operation", addTermTerm);
			addTermTerm.struct = noType;
			return;
		}
		addTermTerm.struct = intType;
		
	}
	
	@Override
	public void visit(Expr expr) {
		if(expr.getAddTerm() instanceof AddTermTerm) {
			if(!expr.getAddTerm().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + expr.getAddTerm().struct + "  in expr", expr);
				expr.struct = noType;
				return;
			} 
			if(!expr.getTerm().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + expr.getTerm().struct + "  in expr", expr);
				expr.struct = noType;
				return;
			}
		}
		expr.struct = expr.getTerm().struct;
	}
	
//	Designator
	
	@Override
	public void visit(DesignatorVar designatorVar) {
		Obj desObj = Tab.find(designatorVar.getDName());
		if(desObj == Tab.noObj) {
			error_report("Accessing undefined var: " + designatorVar.getDName(), designatorVar);
			designatorVar.obj = Tab.noObj;
			return;
		} 
		if(desObj.getKind() != Obj.Var && desObj.getKind() != Obj.Con) {
			error_report("Unadequate variable: " + designatorVar.getDName(), designatorVar);
			designatorVar.obj = Tab.noObj;
			return;			
		}
		designatorVar.obj = desObj;
	}
	
	@Override
	public void visit(DesignatorArr designatorArr) {
		if(designatorArr.getDesignatorArrName().obj == Tab.noObj) {
			designatorArr.obj = Tab.noObj;
			return;
		}
		if(!designatorArr.getExpr().struct.equals(intType)) {
			error_report("Indexing with non-int value", designatorArr);
			designatorArr.obj = Tab.noObj;
			return;
		}
//		In runtime for [$]
		designatorArr.obj = new Obj(Obj.Elem, designatorArr.getDesignatorArrName().obj.getName() + "[$]", designatorArr.getDesignatorArrName().obj.getType().getElemType());
	}
	
	@Override
	public void visit(DesignatorArrName designatorArrName) {
//		int niz[];
//		visit(niz) -> obj(var[niz]) -> strukt arr -> strukt int
//		niz[5];
//		visit niz[] -> elem(strukt int)
		Obj desObj = Tab.find(designatorArrName.getDName());
		if(desObj == Tab.noObj) {
			error_report("Accessing undefined array: " + designatorArrName.getDName(), designatorArrName);
			designatorArrName.obj = Tab.noObj;
			return;
		} 
		if(desObj.getKind() != Obj.Var || desObj.getType().getKind() != Struct.Array) {
			error_report("Unadequate variable, not array: " + designatorArrName.getDName(), designatorArrName);
			designatorArrName.obj = Tab.noObj;
			return;			
		}
		designatorArrName.obj = desObj;
	}

//	Factor
	
	@Override
	public void visit(FactorOpNew factorOpNew) {
		if(!factorOpNew.getExpr().struct.equals(intType)) {
			error_report("Size of array is not of integer type", factorOpNew);
			factorOpNew.struct = noType;
			return;
		}
//		As we previously defined, type is saved to local variable when it's visited
		factorOpNew.struct = new Struct(Struct.Array, factorOpNew.getType().struct);
	}
	
	@Override
	public void visit(FactorOpExpr factorOpExpr) {
		factorOpExpr.struct = factorOpExpr.getExpr().struct;
	}

	@Override
	public void visit(FactorOpBool factorBool) {
		factorBool.struct = boolType;
		info_report("Bool value: " + factorBool.getFactVal() + " used", factorBool);
	}

	@Override
	public void visit(FactorOpNumber factorNumber) {
		factorNumber.struct = intType;
		info_report("Constant value: " + factorNumber.getFactVal() + " used", factorNumber);
	}
	
	@Override
	public void visit(FactorOpDesignator factorOpDesignator) {
		Obj obj = factorOpDesignator.getDesignator().obj;
		factorOpDesignator.struct = obj.getType();
		if(obj.getKind() == Obj.Con) {
			info_report("Constant: " + obj.getName() + " used from symbol table: " + obj.toString(), factorOpDesignator);
		} else if(obj.getKind() == Obj.Var) {
			if(obj.getLevel() == 0) 
				info_report("Global variable: " + obj.getName() + " used from symbol table: " + obj.toString(), factorOpDesignator);
			else 
				info_report("Local variable: " + obj.getName() + " used from symbol table: " + obj.toString(), factorOpDesignator);
		} else if(obj.getKind() == Obj.Elem) {
			info_report("Array element: " + obj.getName() + " used from symbol table: " + obj.toString(), factorOpDesignator);
		}
	}
	
	@Override
	public void visit(FactorOpChar factorChar) {
		factorChar.struct = charType;
		info_report("Char value: " + factorChar.getFactVal() + " used", factorChar);
	}
	
	@Override
	public void visit(FactorOpRange factorOpRange) {
		if(!factorOpRange.getExpr().struct.equals(intType)) {
			error_report("Expression must be of type integer in function range", factorOpRange);
			factorOpRange.struct = noType;
			return;
		}
		factorOpRange.struct = new Struct(Struct.Array, intType);
	}

	
	@Override
	public void visit(Factor factor) {
		if(factor.getFactorSign() instanceof FactorSignPlus) {
			factor.struct = factor.getFactorOp().struct;
			return;
		} 
		if(factor.getFactorOp().struct.equals(intType)) {
			factor.struct = intType;
			return;
		}
		error_report("Negation of non integer value: " + factor.getFactorOp().struct.toString(), factor);
		factor.struct = noType;
	}
	
	
//****************************************************************************************************************
//	Semantic pass code, symbol table
//****************************************************************************************************************
	
	
//	Var declarations
	@Override
	public void visit(VarDeclarationVar varDeclarationVar) {
		Obj varObj = null;
		if(currentMethod == null) {
//			Finds in all scopes, not just open one
//			Returns Tab.noObj;
			varObj = Tab.find(varDeclarationVar.getVName());
		} else {
//			returns null
			varObj = Tab.currentScope().findSymbol(varDeclarationVar.getVName());
		}
		if(varObj != null)
			if(varObj != Tab.noObj) {
				error_report("Double declaration of variable: " + varDeclarationVar.getVName(), varDeclarationVar);
				return;
			} 
		Tab.insert(Obj.Var, varDeclarationVar.getVName(), currentType);
		if(currentMethod == null)
			info_report("Global var declaration: " + varDeclarationVar.getVName(), varDeclarationVar);
		else 
			info_report("Local var declaration: " + varDeclarationVar.getVName() + " in method: " + currentMethod.getName(), varDeclarationVar);
	}
	
	@Override
	public void visit(VarDeclarationArray varDeclarationArray) {
		Obj varObj = null;
		if(currentMethod == null) {
//			Finds in all scopes, not just open one
//			Returns Tab.noObj;
			varObj = Tab.find(varDeclarationArray.getArrName());
		} else {
//			returns null
			varObj = Tab.currentScope().findSymbol(varDeclarationArray.getArrName());
		}
		if(varObj != null)
			if(varObj != Tab.noObj) {
				error_report("Double declaration of array: " + varDeclarationArray.getArrName(), varDeclarationArray);
				return;
			} 
		Tab.insert(Obj.Var, varDeclarationArray.getArrName(), new Struct(Struct.Array, currentType));
		if(currentMethod == null)
			info_report("Global array declaration: " + varDeclarationArray.getArrName(), varDeclarationArray);
		else 
			info_report("Local array declaration: " + varDeclarationArray.getArrName() + " in method: " + currentMethod.getName(), varDeclarationArray);
	}
	
//	Method declarations
	public void visit(MethodName methodName) {
		Obj metObj = null;
		metObj = Tab.find(methodName.getMName());
		if(metObj != Tab.noObj) {
			if(metObj.getKind() == Obj.Meth) {
				error_report("There is method with the name: " + methodName.getMName() + " already defined", methodName);
				currentMethod = null;
				return;
			}
			if(metObj.getKind() == Obj.Var || metObj.getKind() == Obj.Con) {
				error_report("There is variable or const already defined, with the name: " + methodName.getMName(), methodName);
				currentMethod = null;
				return;
			}
		}
		if(methodName.getMName().equals("main")) {
			if(ismain) {
				error_report("Main method declared more than once", methodName);
			}
			ismain = true;
		}			
		methodName.obj = currentMethod = Tab.insert(Obj.Meth, methodName.getMName(), noType);
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
			currentType = noType;
			return;
		}else if(typeObj.getKind() != Obj.Type) {
			error_report("This: "  + type.getTName() + " is not of type", type);
			currentType = noType;
			return;
		}
		currentType = typeObj.getType();
		type.struct = typeObj.getType();
	}
	
	@Override
	public void visit(TypeConstNum typeConstNum) {
		constant = typeConstNum.getConstVal();
//		constantType = intType;
		typeConstNum.struct = intType;
	}
	
	@Override
	public void visit(TypeConstChar typeConstChar) {
		constant = typeConstChar.getConstVal();
//		constantType = charType;
		typeConstChar.struct = charType;
	}
	
	@Override
	public void visit(TypeConstBool typeConstBool) {
		constant = typeConstBool.getConstVal();
//		Tab.find(bool)
//		constantType = boolType;
		typeConstBool.struct = boolType;
	}
	
	@Override
	public void visit(ConstDeclAssign constDeclAssign) {
//		Is constant already in the symbol table
		if(Tab.find(constDeclAssign.getVName()) != Tab.noObj) {
			error_report("Double definition of constant: " + constDeclAssign.getVName(), constDeclAssign);
			return;
		} else if(!constDeclAssign.getTypeConst().struct.assignableTo(currentType)) {
			error_report("Trying to assign: " + constantType.getKind() + " to: " + currentType.getKind(), constDeclAssign);
			return;
		}
//		Equals, compatible with, assignable to with struct types comparison
//		We can go through parents, but we would need to do it in while loop, because we don't know where Type is
//		Because we can declare only one type in one declaration[const int pi, e, g;] we can save that type and then use it here
		Obj conObj = Tab.insert(Obj.Con, constDeclAssign.getVName(), currentType);
//		We must write value to adr field for constants
		conObj.setAdr(constant);
		info_report("Constant declaration: " + constDeclAssign.getVName(), constDeclAssign);
	}
	
//	Always for concrete classes, not abstract
	@Override
	public void visit(Program program) {
//		We must count global variables
		nVars = Tab.currentScope().getnVars();
		
//		Closing scope on end
//		If we do find, it will take [name] from current scope
		Tab.chainLocalSymbols(myProg);
		Tab.closeScope();
		myProg = null;
		if(!ismain)
			error_report("No main method in the program", program);
	}
	
	@Override
	public void visit(ProgramName programName) {
//		Creating program scope
		myProg = Tab.insert(Obj.Prog, programName.getPName(), noType);
		Tab.openScope();
	}
	
	
//****************************************************************************************************************	
//	Semantic reporting code, and helper functions
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
	
	public int getNVars() {
		return nVars;
	}
//****************************************************************************************************************
}
