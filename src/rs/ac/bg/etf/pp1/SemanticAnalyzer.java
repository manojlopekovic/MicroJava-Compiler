package rs.ac.bg.etf.pp1;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.Vector;

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
	private boolean returnedFromCurrentMethod = false;
	private int nVars;
	private Obj calledMethod = Tab.noObj;
	private Stack<Obj> methodStack = new Stack<>(); 
	private boolean forLoop = false;
	private Stack<Boolean> forStack = new Stack<>();


//****************************************************************************************************************
//	Semantic pass code, context conditions
//****************************************************************************************************************

//****************************************************************************************************************
//	For loop
//****************************************************************************************************************	
	
	@Override
	public void visit(ForStart forStart) {
		if(forLoop)
			forStack.push(forLoop);
		forLoop = true;
	}
	
	@Override
	public void visit(StmtFor stmtFor) {
		if(forStack.empty())
			forLoop = false;
		else 
			forLoop = forStack.pop();
	}
	
	@Override
	public void visit(StmtBreak bStmtBreak) {
		if(!forLoop) {
			error_report("Break found outside of for loop", bStmtBreak);
		}
	}
	
	@Override
	public void visit(StmtContinue cStmtContinue) {
		if(!forLoop) {
			error_report("Continue found outside of for loop", cStmtContinue);
		}
	}
	
	private List<Obj> exprHasDesObj(Expr expr) {
		List<Obj> list = new ArrayList<>(); 
		List<Obj> termList = termHasDesObj(expr.getTerm());
		list.addAll(termList);
		if(expr.getAddTerm() instanceof AddTermTerm) {
			List<Obj> addTermList = addTermHasDesObj((AddTermTerm) expr.getAddTerm()); 
			list.addAll(addTermList);
		}
		return list;
	}
	
	private List<Obj> addTermHasDesObj(AddTermTerm term) {
		List<Obj> list = new ArrayList<>(); 
		List<Obj> termList = termHasDesObj(term.getTerm());
		list.addAll(termList);
		if(term.getAddTerm() instanceof AddTermTerm) {
			List<Obj> addTermList = addTermHasDesObj((AddTermTerm) term.getAddTerm()); 
			list.addAll(addTermList);
		}
		return list;
	}
	
	private List<Obj> termHasDesObj(Term term) {
		List<Obj> list = new ArrayList<>(); 
		List<Obj> factList = factorHasDesObj(term.getFactor().getFactorOp());
		list.addAll(factList);
		if(term.getMulFactor() instanceof MulFactorFactor) {
			List<Obj> mulfacList = mulFactHasDesObj((MulFactorFactor) term.getMulFactor());
			list.addAll(mulfacList);
		}
		return list;
	}
	
	private List<Obj> mulFactHasDesObj(MulFactorFactor term) {
		List<Obj> list = new ArrayList<>(); 
		List<Obj> factList = factorHasDesObj(term.getFactor().getFactorOp());
		list.addAll(factList);
		if(term.getMulFactor() instanceof MulFactorFactor) {
			List<Obj> mulfacList = mulFactHasDesObj((MulFactorFactor) term.getMulFactor());
			list.addAll(mulfacList);
		}
		return list;
	}
	
	private List<Obj> exprListHasDesObj(ExprListExpr expr) {
		List<Obj> list = new ArrayList<>(); 
		List<Obj> exprList = exprHasDesObj(expr.getExpr());
		list.addAll(exprList);
		if(expr.getExprList() instanceof ExprListExpr) {
			exprList = exprListHasDesObj((ExprListExpr) expr.getExprList());
			list.addAll(exprList);
		}
		return list;
	}
	
	private List<Obj> actParsHasDesObj(ActParsExpr actParsExpr) {
		List<Obj> list = new ArrayList<>(); 
		List<Obj> exprList = exprHasDesObj(actParsExpr.getExpr());
		list.addAll(exprList);
		if(actParsExpr.getExprList() instanceof ExprListExpr) {
			exprList = exprListHasDesObj((ExprListExpr) actParsExpr.getExprList());
			list.addAll(exprList);
		}
		return list;
	}
	
	private List<Obj> factorHasDesObj(FactorOp factorOp) {
		List<Obj> list = new ArrayList<>(); 
		if(factorOp instanceof FactorOpDesignator) {
			FactorOpDesignator factorOpDesignator = (FactorOpDesignator) factorOp;
			list.add(factorOpDesignator.getDesignator().obj);
		} else if(factorOp instanceof FactorOpExpr) {
			List<Obj> exprList = exprHasDesObj(((FactorOpExpr) factorOp).getExpr());
			if(exprList != null)
				list.addAll(exprList);
		} else if(factorOp instanceof FactorOpMethCall) {
			FactorOpMethCall factorOpMethCall = (FactorOpMethCall) factorOp;
			if(factorOpMethCall.getActPars() instanceof ActParsExpr) {
				List<Obj> parsList = actParsHasDesObj((ActParsExpr) factorOpMethCall.getActPars());
				list.addAll(parsList);
			}
		}
		return list;
	}
	
	@Override
	public void visit(DStmtListComp listComp) {
		if(listComp.getDesignator().obj.getType().getKind() != listComp.getListCompDes().getDesignator().obj.getType().getKind()){
			error_report("Kinds in list comperhension are: " + listComp.getDesignator().obj.getType().getKind()
					+ " and: " + listComp.getListCompDes().getDesignator().obj.getType().getKind(), listComp);
			return;
		}
		if(!listComp.getDesignator().obj.getType().equals(listComp.getListCompDes().getDesignator().obj.getType())) {
			error_report("Tried to do comperhension for array of type: " + listComp.getDesignator().obj.getType().getElemType().getKind() 
					+ " with array of type: " + listComp.getListCompDes().getDesignator().obj.getType().getElemType().getKind(), listComp);
			return;	
		}
		if(!listComp.getListCompExpr().getExpr().struct.equals(listComp.getListCompDes().getDesignator().obj.getType().getElemType())) {
			error_report("In list comperhension, expression type: " + listComp.getListCompExpr().getExpr().struct.getKind() + ", doesn't match designator type: " + 
						listComp.getListCompDes().getDesignator().obj.getType().getElemType().getKind(), listComp);
			return;
		}
//		Now, i have to count number of variables in expr, and it cannot be more than 1
		List<Obj> objList = exprHasDesObj(listComp.getListCompExpr().getExpr());
		if(!objList.isEmpty()) {
			boolean multipleObj = false;
			Obj firstObj = objList.get(0);
			for (Obj obj : objList) {
				if(!obj.equals(firstObj)) {
					multipleObj = true;
					break;
				}
			}
			if(multipleObj) {
				error_report("Multiple variables declared in expression inside list comperhension", listComp);
				return;
			}
		}
	}
	
	
	private List<Obj> condHasDesObj(ConditionListCompNoErr cond){
		List<Obj> list = new ArrayList<>(); 
		list.addAll(condTermHasDesObj(cond.getListCompConditions().getCondTerm()));
		if(cond.getListCompConditions().getOrCondTerm() instanceof OrCondTermOr) {
			list.addAll(orCondTermHasDesObj((OrCondTermOr) cond.getListCompConditions().getOrCondTerm()));
		}
		return list;
	}
	
	private List<Obj> orCondTermHasDesObj(OrCondTermOr orCondTerm) {
		List<Obj> list = new ArrayList<>(); 
		list.addAll(condTermHasDesObj(orCondTerm.getCondTerm()));
		if(orCondTerm.getOrCondTerm() instanceof OrCondTermOr) {
			list.addAll(orCondTermHasDesObj((OrCondTermOr) orCondTerm.getOrCondTerm()));
		}
		return list;
	}
	
	private List<Obj> condTermHasDesObj(CondTerm condTerm) {
		List<Obj> list = new ArrayList<>(); 
		list.addAll(condfactorHasDesObj(condTerm.getCondFact()));
		if(condTerm.getAndCondFact() instanceof AndCondFactAnd) {
			list.addAll(andCondFactHasDesObj((AndCondFactAnd) condTerm.getAndCondFact()));
		}
		return list;
	}
	
	private List<Obj> andCondFactHasDesObj(AndCondFactAnd andCondFactAnd) {
		List<Obj> list = new ArrayList<>(); 
		list.addAll(condfactorHasDesObj(andCondFactAnd.getCondFact()));
		if(andCondFactAnd.getAndCondFact() instanceof AndCondFactAnd) {
			list.addAll(andCondFactHasDesObj((AndCondFactAnd) andCondFactAnd.getAndCondFact()));
		}
		return list;
	}
	
	private List<Obj> condfactorHasDesObj(CondFact condFact) {
		List<Obj> list = new ArrayList<>(); 
		if(condFact instanceof CondFactExpr) {
			CondFactExpr condFactExpr = (CondFactExpr) condFact;
			list.addAll(exprHasDesObj(condFactExpr.getExpr()));
		} else {
			CondFactRelop condFactRelop = (CondFactRelop) condFact;
			list.addAll(exprHasDesObj(condFactRelop.getExpr()));
			list.addAll(exprHasDesObj(condFactRelop.getExpr1()));
		}		
		return list;
	}
	
	@Override
	public void visit(DStmtListCompIf listCompIf) {
		if(listCompIf.getDesignator().obj.getType().getKind() != listCompIf.getListCompDes().getDesignator().obj.getType().getKind()){
			error_report("Kinds in list comperhension are: " + listCompIf.getDesignator().obj.getType().getKind()
					+ " and: " + listCompIf.getListCompDes().getDesignator().obj.getType().getKind(), listCompIf);
			return;
		}
		if(!listCompIf.getDesignator().obj.getType().equals(listCompIf.getListCompDes().getDesignator().obj.getType())) {
			error_report("Tried to do comperhension for array of type: " + listCompIf.getDesignator().obj.getType().getElemType().getKind() 
					+ " with array of type: " + listCompIf.getListCompDes().getDesignator().obj.getType().getElemType().getKind(), listCompIf);
			return;	
		}
		if(!listCompIf.getListCompExpr().getExpr().struct.equals(listCompIf.getListCompDes().getDesignator().obj.getType().getElemType())) {
			error_report("In list comperhension, expression type: " + listCompIf.getListCompExpr().getExpr().struct.getKind() + ", doesn't match designator type: " + 
					listCompIf.getListCompDes().getDesignator().obj.getType().getElemType().getKind(), listCompIf);
			return;
		}
//		Now, i have to count number of variables in expr, and it cannot be more than 1
		List<Obj> objList = exprHasDesObj(listCompIf.getListCompExpr().getExpr());
		boolean multipleObj = false;
		Obj firstObj = null;
		if(!objList.isEmpty()) {
			firstObj = objList.get(0);
			for (Obj obj : objList) {
				if(!obj.equals(firstObj)) {
					multipleObj = true;
					break;
				}
			}
			if(multipleObj) {
				error_report("Multiple variables declared in expression inside list comperhension", listCompIf);
				return;
			}
		}
//		Then, i have to check for the same variable in condition, if it's there
		if(listCompIf.getConditionListComp() instanceof ConditionListCompNoErr) {
			ConditionListCompNoErr compNoErr = (ConditionListCompNoErr) listCompIf.getConditionListComp();
			List<Obj> condObjList = condHasDesObj(compNoErr); 
			if(!condObjList.isEmpty()) {
				multipleObj = false;
				Obj condObj = condObjList.get(0);
				for (Obj obj : condObjList) {
					if(!obj.equals(condObj)) {
						multipleObj = true;
						break;
					}
				}
				if(multipleObj || (firstObj != null && !firstObj.equals(condObj))) {
					error_report("There are multiple variables declared in condition inside list comperhension", listCompIf);
					return;
				}
			}
		}
	}
	
	@Override
	public void visit(ConditionListCompNoErr condition) {
		if(condition.getListCompConditions().getOrCondTerm() instanceof OrCondTermOr) {
			OrCondTermOr orCondTermOr = (OrCondTermOr) condition.getListCompConditions().getOrCondTerm();
			if(!condition.getListCompConditions().getOrCondTerm().struct.compatibleWith(boolType)) {
				error_report("Expressions are not compatible in condition[ " + condition.getListCompConditions().getOrCondTerm().struct.getKind() + ", " + boolType.getKind() + " ]" , condition);
				condition.struct = noType;
				return;
			}
		}
		condition.struct = boolType;
	}
	
//****************************************************************************************************************
//	If, Else, Conditions
//****************************************************************************************************************	
	
	@Override
	public void visit(ConditionNoErr condition) {
		if(condition.getOrCondTerm() instanceof OrCondTermOr) {
			OrCondTermOr orCondTermOr = (OrCondTermOr) condition.getOrCondTerm();
			if(!condition.getOrCondTerm().struct.compatibleWith(boolType)) {
				error_report("Expressions are not compatible in condition[ " + condition.getOrCondTerm().struct.getKind() + ", " + boolType.getKind() + " ]" , condition);
				condition.struct = noType;
				return;
			}
		}
		condition.struct = boolType;
	}
	
	@Override
	public void visit(OrCondTermOr orCondTerm) {
		if(orCondTerm.getOrCondTerm() instanceof OrCondTermOr) {
			if(!orCondTerm.getOrCondTerm().struct.compatibleWith(orCondTerm.getCondTerm().struct)) {
				error_report("Expressions are not compatible in or comparisons[ " + orCondTerm.getOrCondTerm().struct.getKind() + ", " + orCondTerm.getCondTerm().struct.getKind() + " ]" , orCondTerm);
				orCondTerm.struct = noType;
				return;
			}
		}
		orCondTerm.struct = orCondTerm.getCondTerm().struct;
	}
	
	@Override
	public void visit(CondTerm condTerm) {
		if(condTerm.getAndCondFact() instanceof AndCondFactAnd) {
			AndCondFactAnd andCondFactAnd = (AndCondFactAnd) condTerm.getAndCondFact();
			if(!condTerm.getCondFact().struct.compatibleWith(andCondFactAnd.struct)) {
				error_report("Expressions are not compatible in conditional term[ " + condTerm.getCondFact().struct.getKind() + ", " + andCondFactAnd.struct.getKind() + " ]" , condTerm);
				condTerm.struct = noType;
				return;
			}
		}
		condTerm.struct = condTerm.getCondFact().struct;
	}
	
	@Override
	public void visit(AndCondFactAnd andCondFact) {
		if(andCondFact.getAndCondFact() instanceof AndCondFactAnd) {
			if(!andCondFact.getAndCondFact().struct.compatibleWith(andCondFact.getCondFact().struct)) {
				error_report("Expressions are not compatible in and comparisons[ " + andCondFact.getAndCondFact().struct.getKind() + ", " + andCondFact.getCondFact().struct.getKind() + " ]" , andCondFact);
				andCondFact.struct = noType;
				return;
			}
		}
		andCondFact.struct = andCondFact.getCondFact().struct;
	}
	
	@Override
	public void visit(CondFactRelop condFact) {
		if(!condFact.getExpr().struct.compatibleWith(condFact.getExpr1().struct)) {
			error_report("Expressions not compatible in conditional fact[ " + condFact.getExpr().struct.getKind() + ", " + condFact.getExpr1().struct.getKind() + " ]" , condFact);
			condFact.struct = noType;
			return;
		}
		if(condFact.getExpr().struct.getKind() == Struct.Array) {
			if(!((condFact.getRelop() instanceof RelopEquals) || condFact.getRelop() instanceof RelopNotEquals)) {
				error_report("Relational operator other than == or != used with array type", condFact);
				condFact.struct = noType;
				return;
			}
		}
		condFact.struct = boolType;
	}
	
	@Override
	public void visit(CondFactExpr condFact) {
		if(condFact.getExpr().struct == boolType) {
			condFact.struct = condFact.getExpr().struct;
			return;
		}
		error_report("Expression in condition is not boolean", condFact);
		condFact.struct = Tab.noType;
	}
	
	
//****************************************************************************************************************
//	Statement
//****************************************************************************************************************
	
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
		if(stmtPrint.getExpr().getTerm().getFactor().getFactorOp() instanceof FactorOpMethCall) {
			FactorOpMethCall factorOpMethCall = (FactorOpMethCall) stmtPrint.getExpr().getTerm().getFactor().getFactorOp();
			typeStruct = factorOpMethCall.getMethodCallName().getDesignator().obj.getType();
		}
		if(typeStruct.getKind() != Struct.Array && !typeStruct.equals(intType) && !typeStruct.equals(boolType)  && !typeStruct.equals(charType)) {
			error_report("Trying to print: " + typeStruct.getKind() + " which is not array, integer, char or bool type", stmtPrint);
			return;
		}
	}
	
	@Override
	public void visit(StmtPrintNum stmtPrint) {
//		Check if variable, element of an array [Field inside class for C]
		Struct typeStruct = stmtPrint.getExpr().struct;
		if(stmtPrint.getExpr().getTerm().getFactor().getFactorOp() instanceof FactorOpMethCall) {
			FactorOpMethCall factorOpMethCall = (FactorOpMethCall) stmtPrint.getExpr().getTerm().getFactor().getFactorOp();
			typeStruct = factorOpMethCall.getMethodCallName().getDesignator().obj.getType();
		}
		if(typeStruct.getKind() != Struct.Array && !typeStruct.equals(intType) && !typeStruct.equals(boolType)  && !typeStruct.equals(charType)) {
			error_report("Trying to print: " + typeStruct.getKind() + " which is not array, integer, char or bool type, with params", stmtPrint);
			return;
		}
	}
	
	@Override
	public void visit(StmtRet stmtRet) {
		if(currentMethod == null || currentMethod == Tab.noObj) {
			error_report("Return statement outside of method", stmtRet);
			return;
		}
		if(stmtRet.getExprEpsilon() instanceof ExprEpsilonExpr) {
			ExprEpsilonExpr expr = (ExprEpsilonExpr) stmtRet.getExprEpsilon();
			if(!expr.getExpr().struct.equals(currentMethod.getType())) {
				error_report("Trying to return type: " + expr.getExpr().struct.getKind() + " but expecting type: " + currentMethod.getType().getKind(), expr);
			}
			returnedFromCurrentMethod = true;
			return;
		}
		if(currentMethod != null && currentMethod != Tab.noObj) {
			if(currentMethod.getType() != noType)
				error_report("Return of non void value for void method", stmtRet);
		}
	}
	
	
//****************************************************************************************************************
//	Designator statement
//****************************************************************************************************************
	
	@Override
	public void visit(DStmtDesMeth methodCall) {
		if(methodStack.empty())
			calledMethod = Tab.noObj;
		else 
			calledMethod = methodStack.pop();
	}
	
	@Override
	public void visit(DStmtDesAExpr dStmtDesAExpr) {
//		Check if variable, element of an array [Field inside class for C]
		if(dStmtDesAExpr.getDesignator().obj.getKind() != Obj.Var && dStmtDesAExpr.getDesignator().obj.getKind() != Obj.Elem) {
			error_report("Trying to assign to: " + dStmtDesAExpr.getDesignator().obj.getName() + " which is of unassignable type", dStmtDesAExpr);
			return;
		}
		if(!dStmtDesAExpr.getExpr().struct.assignableTo(dStmtDesAExpr.getDesignator().obj.getType())) {
			error_report("Trying to assign variable type: " + dStmtDesAExpr.getExpr().struct.getKind() + " to: " + dStmtDesAExpr.getDesignator().obj.getName() + ", which is of type: " + dStmtDesAExpr.getDesignator().obj.getType().getKind(), dStmtDesAExpr);
			return;
		}
		if(dStmtDesAExpr.getExpr().getTerm().getFactor().getFactorOp() instanceof FactorOpDesignator) {
			FactorOpDesignator factorOpDesignator = (FactorOpDesignator) dStmtDesAExpr.getExpr().getTerm().getFactor().getFactorOp();
			if(factorOpDesignator.getDesignator().obj.getKind() == Obj.Meth) {
				error_report("Method name: " + factorOpDesignator.getDesignator().obj.getName() + " cannot be called withoud parenthesis", factorOpDesignator);
				return;
			}
		}
		if(dStmtDesAExpr.getDesignator().obj.getFpPos() == 1) {
			info_report("Assignment to formal parameter: " + dStmtDesAExpr.getDesignator().obj.getName() + " in method: " + currentMethod.getName(), dStmtDesAExpr);
		} else if(dStmtDesAExpr.getDesignator().obj.getKind() == Obj.Var) {
			if(dStmtDesAExpr.getDesignator().obj.getLevel() == 0) 
				info_report("Assignment to global parameter: " + dStmtDesAExpr.getDesignator().obj.getName() + " in method: " + currentMethod.getName(), dStmtDesAExpr);
			else 
				info_report("Assignment to local parameter: " + dStmtDesAExpr.getDesignator().obj.getName() + " in method: " + currentMethod.getName(), dStmtDesAExpr);
		} else if(dStmtDesAExpr.getDesignator().obj.getKind() == Obj.Elem) {
			info_report("Assignment to array element: " + dStmtDesAExpr.getDesignator().obj.getName() + " in method: " + currentMethod.getName(), dStmtDesAExpr);
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
	
	
	
//****************************************************************************************************************
//	Expression and Term
//****************************************************************************************************************
	
	@Override
	public void visit(MulFactorFactor mulFactorFactor) {
		if(mulFactorFactor.getMulFactor() instanceof MulFactorFactor) {
			if(!mulFactorFactor.getMulFactor().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + mulFactorFactor.getMulFactor().struct.getKind() + " in multiplication operation", mulFactorFactor);
				mulFactorFactor.struct = noType;
				return;
			}
		}
		if(!mulFactorFactor.getFactor().struct.equals(intType)) {
			error_report("Trying to assign non-int value: " + mulFactorFactor.getFactor().struct.getKind() + " in multiplication operation", mulFactorFactor);
			mulFactorFactor.struct = noType;
			return;
		}
		mulFactorFactor.struct = intType;
	}
	
	@Override
	public void visit(Term term) {
		if(term.getMulFactor() instanceof MulFactorFactor) {
			if(!term.getMulFactor().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + term.getMulFactor().struct.getKind() + "  in term", term);
				term.struct = noType;
				return;
			} 
			if(!term.getFactor().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + term.getFactor().struct.getKind() + "  in term", term);
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
				error_report("Trying to assign non-int value: " + addTermTerm.getAddTerm().struct.getKind() + " in addition operation", addTermTerm);
				addTermTerm.struct = noType;
				return;
			}
		}
		if(!addTermTerm.getTerm().struct.equals(intType)) {
			error_report("Trying to assign non-int value: " + addTermTerm.getTerm().struct.getKind() + " in addition operation", addTermTerm);
			addTermTerm.struct = noType;
			return;
		}
		addTermTerm.struct = intType;
		
	}
	
	@Override
	public void visit(Expr expr) {
		if(expr.getAddTerm() instanceof AddTermTerm) {
			if(!expr.getAddTerm().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + expr.getAddTerm().struct.getKind() + "  in expr", expr);
				expr.struct = noType;
				return;
			} 
			if(!expr.getTerm().struct.equals(intType)) {
				error_report("Trying to assign non-int value: " + expr.getTerm().struct.getKind() + "  in expr", expr);
				expr.struct = noType;
				return;
			}
		}
		expr.struct = expr.getTerm().struct;
	}
	
	@Override
	public void visit(ExprEpsilon expr) {
		if(expr instanceof ExprEpsilonExpr)
			expr.struct = ((ExprEpsilonExpr) expr).getExpr().struct;
		else 
			expr.struct = Tab.noType;
	}
	
//****************************************************************************************************************
//	Designator
//****************************************************************************************************************
	
	@Override
	public void visit(DesignatorVar designatorVar) {
		Obj desObj = Tab.find(designatorVar.getDesName());
		if(desObj == Tab.noObj) {
			error_report("Accessing undefined var: " + designatorVar.getDesName(), designatorVar);
			designatorVar.obj = Tab.noObj;
			return;
		} 
		if(desObj.getKind() != Obj.Var && desObj.getKind() != Obj.Con && desObj.getKind() != Obj.Meth) {
			error_report("Unadequate variable: " + designatorVar.getDesName(), designatorVar);
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
		Obj desObj = Tab.find(designatorArrName.getDesArrName());
		if(desObj == Tab.noObj) {
			error_report("Accessing undefined array: " + designatorArrName.getDesArrName(), designatorArrName);
			designatorArrName.obj = Tab.noObj;
			return;
		} 
		if(desObj.getKind() != Obj.Var || desObj.getType().getKind() != Struct.Array) {
			error_report("Unadequate variable, not array: " + designatorArrName.getDesArrName(), designatorArrName);
			designatorArrName.obj = Tab.noObj;
			return;			
		}
		designatorArrName.obj = desObj;
	}

//****************************************************************************************************************
//	Factor
//****************************************************************************************************************
	
	@Override
	public void visit(FactorOpMethCall factorMethCall) {
		if(factorMethCall.getMethodCallName().getDesignator() instanceof DesignatorVar) {
			DesignatorVar designatorVar = (DesignatorVar) factorMethCall.getMethodCallName().getDesignator();
			factorMethCall.struct = designatorVar.obj.getType();
		} else {
			factorMethCall.struct = noType;
		}
		if(methodStack.empty())
			calledMethod = Tab.noObj;
		else
			calledMethod = methodStack.pop();
	}
	
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
		if(obj != Tab.noObj) {
			if(obj.getFpPos() == 1) {
				info_report("Used formal parameter: " + obj.getName() + ", of method: " + currentMethod.getName(), factorOpDesignator);
			} else if(obj.getKind() == Obj.Con) {
				info_report("Constant: " + obj.getName() + " used from symbol table", factorOpDesignator);
			} else if(obj.getKind() == Obj.Var) {
				if(obj.getLevel() == 0) 
					info_report("Global variable: " + obj.getName() + " used from symbol table", factorOpDesignator);
				else 
					info_report("Local variable: " + obj.getName() + " used from symbol table", factorOpDesignator);
			} else if(obj.getKind() == Obj.Elem) {
				info_report("Array element: " + obj.getName() + " used from symbol table", factorOpDesignator);
			}
			
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
	
	
//****************************************************************************************************************
//	Variables
//****************************************************************************************************************
	
	@Override
	public void visit(VarDeclarationVar varDeclarationVar) {
		Obj varObj = null;
		if(currentMethod == null) {
//			Finds in all scopes, not just open one
//			Returns Tab.noObj;
			varObj = Tab.find(varDeclarationVar.getVarName());
		} else {
//			returns null
			varObj = Tab.currentScope().findSymbol(varDeclarationVar.getVarName());
		}
		if(varObj != null)
			if(varObj != Tab.noObj) {
				error_report("Double declaration of variable: " + varDeclarationVar.getVarName(), varDeclarationVar);
				return;
			} 
		Tab.insert(Obj.Var, varDeclarationVar.getVarName(), currentType);
		if(currentMethod == null)
			info_report("Global var declaration: " + varDeclarationVar.getVarName(), varDeclarationVar);
		else 
			info_report("Local var declaration: " + varDeclarationVar.getVarName() + " in method: " + currentMethod.getName(), varDeclarationVar);
	}
	
	@Override
	public void visit(VarDeclarationArr varDeclarationArray) {
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
	
//****************************************************************************************************************
//	Methods
//****************************************************************************************************************
	
	Stack<Obj> formalParamStack = new Stack<>();
	
	@Override
	public void visit(FormParsPars formPars) {
		int i = 1;
		if(formPars.getFormParsList() instanceof FormParsMany) {
			FormParsMany countParsMulRep = (FormParsMany) formPars.getFormParsList();
			i++;
			while(countParsMulRep.getFormParsList() instanceof FormParsMany) {
				i++;
				countParsMulRep = (FormParsMany)countParsMulRep.getFormParsList();
			}
		}
		currentMethod.setLevel(formalParamStack.size());
		while(!formalParamStack.empty()) {
			Obj obj = formalParamStack.pop();
			obj = Tab.insert(obj.getKind(), obj.getName(), obj.getType());
			obj.setFpPos(1);
		}
	}
	
	@Override
	public void visit(FormParsEpsilon formPars) {
		currentMethod.setLevel(0);
	}
	
	@Override
	public void visit(FormsParsDeclArr formsParsDeclArr) {
		Obj arrFormObj = new Obj(Obj.Var, formsParsDeclArr.getVName(), new Struct(Struct.Array, currentType));
		arrFormObj.setFpPos(1);
		formalParamStack.push(arrFormObj);
	}
	
	@Override
	public void visit(FormsParsDeclVar formsParsDeclVar) {
		Obj varFormObj = new Obj(Obj.Var, formsParsDeclVar.getVName(), currentType);
		varFormObj.setFpPos(1);
		formalParamStack.push(varFormObj);
	}
	
	@Override
	public void visit(FormParsMulRep formParsMulRep) {
		Obj varFormObj = new Obj(Obj.Var, formParsMulRep.getVName(), currentType);
		varFormObj.setFpPos(1);
		formalParamStack.push(varFormObj);
	}
	
	@Override
	public void visit(FormParsMulArr formParsMulArr) {
		Obj arrFormObj = new Obj(Obj.Var, formParsMulArr.getVName(), new Struct(Struct.Array, currentType));
		arrFormObj.setFpPos(1);
		formalParamStack.push(arrFormObj);
	}
	
	@Override
	public void visit(ActParsExpr actPars) {
		Collection<Obj> formParsArrayList = calledMethod.getLocalSymbols();
		List<Struct> actParsCollection = new ArrayList<>();
		List<Obj> formParsCollection = new ArrayList<>();
		if(actPars.getExprList() instanceof ExprListExpr) {
			ExprListExpr expr = (ExprListExpr) actPars.getExprList();
			actParsCollection.add(0, expr.getExpr().struct);
			while(expr.getExprList() instanceof ExprListExpr) {
				 expr = (ExprListExpr) expr.getExprList(); 
				 actParsCollection.add(expr.getExpr().struct);
			}
		}
		actParsCollection.add(0, actPars.getExpr().struct);
		if(actParsCollection.size() != calledMethod.getLevel()) {
			error_report("Difference in numbers of formal parameters to actual parameters. Expected: " + calledMethod.getLevel() + " got: " + actParsCollection.size(), actPars);
			return;
		}
		int i = 0;
		for (Obj obj : formParsArrayList) {
			if(obj.getFpPos() == 1)
				formParsCollection.add(obj);
		}
		for (Obj obj : formParsCollection) {
			Struct struct = actParsCollection.get(i);
			if(obj.getType().getKind() != struct.getKind()) {
				error_report("Wrong parameter type for method: " + calledMethod.getName() + ", at position: " + (i + 1) + ", expected: " 
					+ obj.getType().getKind() + ", got: " + struct.getKind(), actPars);
			}
			i++;
		}
	}
	
	@Override
	public void visit(ActParsEpsilon actPars) {
		if(calledMethod != Tab.noObj && calledMethod.getLevel() != 0) {
			error_report("Difference in numbers of formal parameters to actual parameters. Expected: " + calledMethod.getLevel() + " got: " + 0, actPars);
			return;
		}
	}
	
	@Override
	public void visit(MethodCallName methodCallName) {
		if(methodCallName.getDesignator().obj.getKind() != Obj.Meth) {
			error_report("Tried to call identificator: " + methodCallName.getDesignator().obj.getName() + " that does not represent method call", methodCallName);
			return;
		}
		if(calledMethod != Tab.noObj)
			methodStack.push(calledMethod);
		calledMethod = methodCallName.getDesignator().obj;
		info_report("Called method: " + methodCallName.getDesignator().obj.getName(), methodCallName);
	}
	
	public void visit(MethodName methodName) {
//		For A
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
		methodName.obj = currentMethod = Tab.insert(Obj.Meth, methodName.getMName(), currentType);
		Tab.openScope();
		returnedFromCurrentMethod = false;
	}
	
	@Override
	public void visit(MethodDecl methodDecl) {
		if(returnedFromCurrentMethod == false && currentMethod.getType() != noType)
			error_report("There was no return statement for this method", methodDecl);
		Tab.chainLocalSymbols(currentMethod);
		Tab.closeScope();
		currentMethod = null;
		returnedFromCurrentMethod = false;
	}
	
	
//****************************************************************************************************************
//	Constants
//****************************************************************************************************************

//	For B
	@Override
	public void visit(TypeVoidVoid typeVoidVoid) {
		currentType = noType;
	}
//	
	
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
	
//****************************************************************************************************************
//	Program
//****************************************************************************************************************
	
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
//		FpPos is not set for standard methods
		Obj obj = Tab.find("ord");
		Collection<Obj> locSym = obj.getLocalSymbols();
		for (Obj obj2 : locSym) {
			obj2.setFpPos(1);
		}
		obj = Tab.find("chr");
		locSym = obj.getLocalSymbols();
		for (Obj obj2 : locSym) {
			obj2.setFpPos(1);
		}
		obj = Tab.find("len");
		locSym = obj.getLocalSymbols();
		for (Obj obj2 : locSym) {
			obj2.setFpPos(1);
		}
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
