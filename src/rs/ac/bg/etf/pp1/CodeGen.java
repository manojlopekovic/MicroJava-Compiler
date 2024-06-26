package rs.ac.bg.etf.pp1;


import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import javax.swing.plaf.basic.BasicTabbedPaneUI.TabbedPaneLayout;

import java_cup.internal_error;
import rs.ac.bg.etf.pp1.ast.*;
import rs.etf.pp1.mj.runtime.Code;
import rs.etf.pp1.symboltable.Tab;
import rs.etf.pp1.symboltable.concepts.Obj;
import rs.etf.pp1.symboltable.concepts.Struct;

public class CodeGen extends VisitorAdaptor{
	
	private int start_addr = 0;

	public int getStartAddr() {
		return start_addr;
	}

//****************************************************************************************************************
// For loop
//****************************************************************************************************************		
	
	private Stack<Boolean> forLoopStack = new Stack<>();
	private boolean forLoop = false;
	private Stack<Boolean> forLoopCondStack = new Stack<>();
	private boolean forLoopCond = false;
	private Stack<Integer> condAddrStack = new Stack<>();
	private Stack<Integer> secondDesAddrStack = new Stack<>();
	private Stack<Integer> exitAddrStack = new Stack<>();
	private Stack<Integer> stmtAddrStack = new Stack<>();
	
	@Override
	public void visit(DesStmtList desStmtList) {
//		Only before first iteration
		if(!forLoopCond) {
		} 
//		After each iteration
		else {
			
			Code.putJump(condAddrStack.peek());
		}
	}
	
	@Override
	public void visit(FirstSemiSym firstSemiSym) {
//		condAddr = Code.pc;
		condAddrStack.push(Code.pc);
	}
	
	@Override
	public void visit(SecondSemiSym secondSemiSym) {
//		secondDesAddr = Code.pc;
		secondDesAddrStack.push(Code.pc);
	}
	
	@Override
	public void visit(CondFactMayCond codFactMayCond) {
		forLoopCond = true;
		Code.loadConst(1);
		Code.putFalseJump(Code.eq, 0);
//		exitAddr = Code.pc - 2;
		exitAddrStack.push(Code.pc - 2);
		Code.putJump(0);
//		stmtAddr = Code.pc - 2;
		stmtAddrStack.push(Code.pc - 2);
	}
	
	@Override
	public void visit(ForRparenSym forRparenSym) {
//		stmtAddr = Code.pc;
		if(!stmtAddrStack.empty())
			Code.fixup(stmtAddrStack.pop());
	}
	
	@Override
	public void visit(ForEmptyState forEmptyState) {
		Code.putJump(secondDesAddrStack.peek());
	}
	
	@Override
	public void visit(CondFactMayEpsilon condFactMayEpsilon) {
		forLoopCond = true;
		Code.putJump(0);
//		stmtAddr = Code.pc - 2;
		stmtAddrStack.push(Code.pc - 2);
	}
	
	@Override
	public void visit(ForStart forStart) {
		if(forLoop)
			forLoopStack.push(forLoop);
		forLoop = true;
		if(forLoopCond)
			forLoopCondStack.push(forLoopCond);
		forLoopCond = false;
		depth++;
	}
	
	@Override
	public void visit(StmtFor stmtFor) {
//		Code.fixup(exitAddr);
		if(!exitAddrStack.empty())
			Code.fixup(exitAddrStack.pop());
		secondDesAddrStack.pop();
		condAddrStack.pop();
		if(forLoopStack.empty())
			forLoop = false;
		else 
			forLoop = forLoopStack.pop();
		if(!forLoopCondStack.empty())
			forLoopCond = forLoopCondStack.pop();
		if(breaksDepth.get(depth) != null) {
			for (Integer integer : breaksDepth.get(depth)) {
				Code.fixup(integer);
			}
			breaksDepth.clear();
		}
		depth--;
	}
	
	Map<Integer, List<Integer>> breaksDepth = new HashMap<>();
	int depth = 0;
	
	@Override
	public void visit(StmtBreak stmtBreak) {
		if(breaksDepth.get(depth) == null) {
			breaksDepth.put(depth, new ArrayList<>());
		}
		Code.putJump(0);
		breaksDepth.get(depth).add(Code.pc - 2);
	}
	
	@Override
	public void visit(StmtContinue stmtContinue) {
		Code.putJump(secondDesAddrStack.peek());
	}
	
//****************************************************************************************************************
// Conditions
//****************************************************************************************************************		
	
	private Stack<Integer> endPcStack = new Stack<>();
	private Stack<Integer> fixPcStack = new Stack<>();
	
	@Override
	public void visit(ConditionNoErr condition) {
//		When we arrive here, on stack we will have one value, 0 - false, 1 - true
//		Condition is only called after if, so if 1 on stack - continue flow, if 0 on stack - jump
		Code.loadConst(1);
		Code.putFalseJump(Code.eq, 0);
		fixPcStack.push(Code.pc - 2);
	}
	
	@Override
	public void visit(ElseSym elseSym) {
		Code.putJump(0);
		endPcStack.push(Code.pc - 2);
		if(!fixPcStack.empty())
			Code.fixup(fixPcStack.pop());
	}
	
	@Override
	public void visit(IfSym ifSym) {
		if(!fixPcStack.empty())
			fixPcStack.push(fixPcStack.peek());
	}
	
	@Override
	public void visit(EmptyState emptyState) {
		Code.putJump(0);
		endPcStack.push(Code.pc - 2);
		if(!fixPcStack.empty())
			Code.fixup(fixPcStack.pop());
	}
	
	@Override
	public void visit(StmtMatched stmtMatched) {
		Code.fixup(endPcStack.pop());
	}
	
	@Override
	public void visit(UnmatchedIf stmtUnmatchedIf) {
		Code.fixup(endPcStack.pop());
	}
	
	@Override
	public void visit(UnmatchedIfElse stmtUnmatchedIfElse) {
		Code.fixup(endPcStack.pop());
	}
	
	
	private void doCompareRelop() {
		int fix1_pc = Code.pc - 2;
		Code.loadConst(1);
		Code.putJump(0);
		int fix_pc = Code.pc - 2;
		Code.fixup(fix1_pc);
		Code.loadConst(0);
		Code.fixup(fix_pc);
	}
	
	@Override
	public void visit(CondFactRelop condFactRelop) {
		if(condFactRelop.getRelop() instanceof RelopEquals) {
			Code.putFalseJump(Code.eq, 0);
			doCompareRelop();
		} else if (condFactRelop.getRelop() instanceof RelopNotEquals) {
			Code.putFalseJump(Code.ne, 0);
			doCompareRelop();
		} else if (condFactRelop.getRelop() instanceof RelopGreater) {
			Code.putFalseJump(Code.gt, 0);
			doCompareRelop();
		} else if (condFactRelop.getRelop() instanceof RelopGreaterEq) {
			Code.putFalseJump(Code.ge, 0);
			doCompareRelop();
		} else if (condFactRelop.getRelop() instanceof RelopLess) {
			Code.putFalseJump(Code.lt, 0);
			doCompareRelop();
		} else if (condFactRelop.getRelop() instanceof RelopLessEq) {
			Code.putFalseJump(Code.le, 0);
			doCompareRelop();
		}
	}
	
	@Override
	public void visit(AndCondFactAnd andCondFactAnd) {
		Code.put(Code.add);
		Code.loadConst(2);
		Code.putFalseJump(Code.eq, 0);
		doCompareRelop();
	}
	
	@Override
	public void visit(CondTerm condTerm) {
	}
	
	@Override
	public void visit(OrCondTermOr orCondTermOr) {
		Code.put(Code.add);
		Code.loadConst(1);
		Code.putFalseJump(Code.ge, 0);
		doCompareRelop();
	}
	
//****************************************************************************************************************
// Statement
//****************************************************************************************************************	
	
	
	@Override
	public void visit(StmtRead stmtRead) {
		if(stmtRead.getDesignator().obj.getType().equals(Tab.intType))
			Code.put(Code.read);
		else 
			Code.put(Code.bread);
		Code.store(stmtRead.getDesignator().obj);
	}
	
	@Override
	public void visit(StmtPrintExpr stmtPrint) {
//		What if array?
		if(stmtPrint.getExpr().getTerm().getFactor().getFactorOp() instanceof FactorOpDesignator) {
			FactorOpDesignator designator = (FactorOpDesignator) stmtPrint.getExpr().getTerm().getFactor().getFactorOp();
			if(designator.struct.getKind() == Struct.Array) {
				Obj obj = new Obj(Obj.Var, "temp", designator.getDesignator().obj.getType());
				Code.put(Code.dup);
//				Store adr in obj
				Code.store(obj);
				
//				At this point, i have address stored/loadable from obj and on stack: |adr
				
				Code.loadConst(0);
				int retpc = Code.pc;
				Code.put(Code.dup2);
//				At this point stack is: |adr ind adr ind
				
				if(stmtPrint.getExpr().struct.equals(Tab.charType)) {
					Code.put(Code.baload);
					Code.loadConst(0);
					Code.put(Code.bprint);
				} else {
					if(stmtPrint.getExpr().getTerm().getFactor().getFactorOp() instanceof FactorOpDesignator) {
						FactorOpDesignator factorOpDesignator = (FactorOpDesignator)stmtPrint.getExpr().getTerm().getFactor().getFactorOp();
						if(factorOpDesignator.getDesignator().obj.getType().getKind() == Struct.Array 
								&& factorOpDesignator.getDesignator().obj.getType().getElemType() == Tab.charType) {

							Code.put(Code.baload);
							Code.loadConst(0);
							Code.put(Code.bprint);
						} else {
							Code.put(Code.aload);
							Code.loadConst(0);
							Code.put(Code.print);
						}
					} else {
						Code.put(Code.aload);
						Code.loadConst(0);
						Code.put(Code.print);
					}
				}
				
//				|adr ind
				Code.loadConst(1);
				Code.put(Code.add);
				Code.put(Code.dup);
				
//				|adr ind+1 ind+1
				Code.load(obj);
				Code.put(Code.arraylength);
				
//				Compare with addrlen
				Code.putFalseJump(Code.ge, retpc);
				
				Code.put(Code.pop);
				Code.put(Code.pop);
				
				
//				Use this to put adr on stack
//				Code.load(obj);
				
				return;
			}
		}
//		Needs ..., val, width on exprStack
		Code.loadConst(0);
		if(stmtPrint.getExpr().struct.equals(Tab.charType))
			Code.put(Code.bprint);
		else 
			Code.put(Code.print);
	}
	
	@Override
	public void visit(StmtPrintNum stmtPrint) {
		Code.loadConst(stmtPrint.getNum());
		if(stmtPrint.getExpr().struct.equals(Tab.charType))
			Code.put(Code.bprint);
		else 
			Code.put(Code.print);
	}
	
//****************************************************************************************************************
//	Designator Statement
//****************************************************************************************************************	

	@Override
	public void visit(DStmtDesAExpr desAExpr) {
		Code.store(desAExpr.getDesignator().obj);
	}
	
	@Override
	public void visit(DStmtDesInc desInc) {
//		if(desInc.getDesignator().obj.getKind() == Obj.Elem)
		if(desInc.getDesignator() instanceof DesignatorArr) {
			DesignatorArr designatorArr = (DesignatorArr) desInc.getDesignator();
			Code.load(designatorArr.getDesignatorArrName().obj);
			Code.put(Code.dup_x2);
			Code.put(Code.pop);
			Code.put(Code.dup_x1);
		}
		Code.load(desInc.getDesignator().obj);
		Code.loadConst(1);
		Code.put(Code.add);
		Code.store(desInc.getDesignator().obj);
	}
	
	@Override
	public void visit(DStmtDesDec desDec) {
		if(desDec.getDesignator() instanceof DesignatorArr) {
			DesignatorArr designatorArr = (DesignatorArr) desDec.getDesignator();
			Code.load(designatorArr.getDesignatorArrName().obj);
			Code.put(Code.dup_x2);
			Code.put(Code.pop);
			Code.put(Code.dup_x1);
		}
		Code.load(desDec.getDesignator().obj);
		Code.loadConst(-1);
		Code.put(Code.add);
		Code.store(desDec.getDesignator().obj);
	}
	
//****************************************************************************************************************
//	Designator
//****************************************************************************************************************
	
	@Override
	public void visit(DesignatorArrName designatorArrName) {
		Code.load(designatorArrName.obj);
	}
	
	@Override
	public void visit(DesignatorVar designatorVar) {
		if(designatorVar.obj.getName().equals("eol")) {
//			Sign for LF[LineFeed]
//			Code.loadConst(10);
		} else if(designatorVar.obj.getName().equals("null")) {
			Code.loadConst(0);
		}
//		else if(designatorVar.obj.getName().equals("space")) {
//			Code.loadConst(32);
//		}
	}

//****************************************************************************************************************
//	List comperhension
//****************************************************************************************************************		
	
	private Obj exprHasObj(Expr expr) {
		Obj obj = null;
		obj = termHasObj(expr.getTerm());
		if(obj == null)
			if(expr.getAddTerm() instanceof AddTermTerm)
				obj = addTermHasObj((AddTermTerm)expr.getAddTerm());
		return obj;
		
	}
	
	private Obj addTermHasObj(AddTermTerm addTermTerm) {
		Obj obj = null;
		if(addTermTerm.getAddTerm() instanceof AddTermTerm)
			obj = addTermHasObj((AddTermTerm)addTermTerm.getAddTerm());
		if(obj == null)
			obj = termHasObj(addTermTerm.getTerm());
		return obj;
	}
	
	private Obj termHasObj(Term term) {
		Obj obj = null;
		obj = factHasObj(term.getFactor());
		if(obj == null)
			if(term.getMulFactor() instanceof MulFactorFactor)
				obj = mulFactHasObj((MulFactorFactor)term.getMulFactor());
		return obj;
	}
	
	private Obj mulFactHasObj(MulFactorFactor mulFactorFactor) {
		Obj obj = null;
		if(mulFactorFactor.getMulFactor() instanceof MulFactorFactor)
			obj = mulFactHasObj((MulFactorFactor)mulFactorFactor.getMulFactor());
		if(obj == null)
			obj = factHasObj(mulFactorFactor.getFactor());
		return obj;
	}
	
	private Obj exprListHasObj(ExprListExpr exprListExpr) {
		Obj obj = null;
		if(exprListExpr.getExprList() instanceof ExprListExpr)
			obj = exprListHasObj((ExprListExpr) exprListExpr.getExprList());
		if(obj == null)
			obj = exprHasObj(exprListExpr.getExpr());
		return obj;
	}
	
	private Obj actParsHasObj(ActParsExpr actParsExpr) {
		Obj obj = null;
		if(actParsExpr.getExprList() instanceof ExprListExpr)
			obj = exprListHasObj((ExprListExpr) actParsExpr.getExprList());
		if(obj == null)
			obj = exprHasObj(actParsExpr.getExpr());
		return obj;
	}
	
	private Obj factHasObj(Factor fact) {
		Obj obj = null;
		if(fact.getFactorOp() instanceof FactorOpDesignator) {
			obj = ((FactorOpDesignator)fact.getFactorOp()).getDesignator().obj;
		} else if(fact.getFactorOp() instanceof FactorOpMethCall) {
//			obj = ((FactorOpMethCall)fact.getFactorOp()).getMethodCallName().getDesignator().obj;
//			obj = ((FactorOpMethCall)fact.getFactorOp()).get;
			if(((FactorOpMethCall)fact.getFactorOp()).getMethodCallName().getDesignator().obj.getType() != Tab.noType) {
				if(((FactorOpMethCall)fact.getFactorOp()).getActPars() instanceof ActParsExpr)
					obj = actParsHasObj((ActParsExpr) ((FactorOpMethCall)fact.getFactorOp()).getActPars());
			}
		}
		return obj;
	}
	
	private int exitAddr = -1;
	private int exitAddr1 = -1;
	private int exprAddr = -1;
	private Struct boolType = Tab.find("bool").getType();
	private Obj fpObj = new Obj(Obj.Var, "fpobj", boolType);
	private int ifAddr = -1;
	private int ifSymAddr;
	private int inAddrJump;
	private int desAddrJump;
	private int gotoCondAddr;
	private int condFalseAddr;
	private int condTrueAddr;
	
	@Override
	public void visit(ListCompLBracket listCompLBracket) {
		if(listCompLBracket.getParent() instanceof DStmtListComp)
			Code.load(((DStmtListComp)(listCompLBracket.getParent())).getListCompDes().getDesignator().obj);
		else 
			Code.load(((DStmtListCompIf)(listCompLBracket.getParent())).getListCompDes().getDesignator().obj);
		Code.put(Code.arraylength);
		Code.loadConst(0);
		Code.putFalseJump(Code.ne, 0);
		exitAddr1 = Code.pc - 2;
		
		Code.putJump(0);
		inAddrJump = Code.pc - 2;
	}
	
//	For is used for exit case, as array can be of zero length
//	Also, if runtime check is needed, do it here
	@Override
	public void visit(ListCompFor listCompFor) {
		exprAddr = Code.pc;
	}
	
	@Override
	public void visit(ListCompIn listCompIn) {
		Code.fixup(inAddrJump);
		Code.loadConst(0);
		Code.put(Code.dup);
	}
	
	@Override
	public void visit(ListCompEpsilon listCompEpsilon) {
	}

	@Override
	public void visit(ListCompIf listCompIf) {
		Code.fixup(gotoCondAddr);
	}
	
	@Override
	public void visit(ListCompConditions listCompConditions) {
		Code.loadConst(1);
		Code.putFalseJump(Code.ne, condTrueAddr);
		Code.loadConst(1);
		Code.put(Code.add);
		Code.putJump(condFalseAddr);
	}
	
//	In expression, i will save value to array that is written to and increment it's index if needed for if
	@Override
	public void visit(ListCompExpr listCompExpr) {
//		val should be stored to primaryDesignator[ind1]
//		| ind1 ind2 val
		Code.put(Code.dup_x2);
//		| val ind1 ind2 val
		Code.put(Code.pop);
//		| val ind1 ind2
		Code.put(Code.dup_x2);
//		| ind2 val ind1 ind2 
		Code.put(Code.pop);
//		| ind2 val ind1 
		Code.put(Code.dup_x2);
//		| ind1 ind2 val ind1 
		Code.put(Code.dup_x1);
//		| ind1 ind2 ind1 val ind1 
		Code.put(Code.pop);
//		| ind1 ind2 ind1 val 
		
		if(listCompExpr.getParent() instanceof DStmtListComp) {
			DStmtListComp dStmtListComp = (DStmtListComp)(listCompExpr.getParent());
			Code.load(dStmtListComp.getDesignator().obj);
		} else {
			DStmtListCompIf dStmtListCompIf = (DStmtListCompIf)(listCompExpr.getParent());
			Code.load(dStmtListCompIf.getDesignator().obj);
		}
//		| ind1 ind2 ind1 val arrAddr
		Code.put(Code.dup_x2);
//		| ind1 ind2 arrAddr ind1 val arrAddr
		Code.put(Code.pop);
//		| ind1 ind2 arrAddr ind1 val
		if(listCompExpr.getExpr().struct.equals(Tab.charType))
			Code.put(Code.bastore);
		else 
			Code.put(Code.astore);
		
//		| ind1 ind2 
		Code.put(Code.dup_x1);
//		| ind2 ind1 ind2 
		Code.put(Code.pop);
//		| ind2 ind1  
		Code.loadConst(1);
		Code.put(Code.add);
//		| ind2 ind1+1  
		Code.put(Code.dup_x1);
//		| ind1+1 ind2 ind1+1  
		Code.put(Code.pop);
//		| ind1+1 ind2  

		condFalseAddr = Code.pc;
//		Check condition
		Code.put(Code.dup);
		if(listCompExpr.getParent() instanceof DStmtListComp)
			Code.load(((DStmtListComp)(listCompExpr.getParent())).getListCompDes().getDesignator().obj);
		else 
			Code.load(((DStmtListCompIf)(listCompExpr.getParent())).getListCompDes().getDesignator().obj);
		Code.put(Code.arraylength);
		Code.putFalseJump(Code.lt, 0);
		exitAddr = Code.pc - 2;

		Code.putJump(0);
		desAddrJump = Code.pc - 2;
		
	}
	
	@Override
	public void visit(ListCompDes listCompDes) {
		Code.fixup(desAddrJump);
		
//		|ind1 ind2 
		Code.put(Code.dup);
		Code.load(listCompDes.getDesignator().obj);
		Code.put(Code.dup_x1);
		Code.put(Code.pop);
		Code.put(Code.aload);
		
		Obj obj;
		if(listCompDes.getParent() instanceof DStmtListComp) {
			DStmtListComp dStmtListComp = (DStmtListComp)(listCompDes.getParent());
			obj = exprHasObj(dStmtListComp.getListCompExpr().getExpr());
		} else {
			DStmtListCompIf dStmtListCompIf = (DStmtListCompIf)(listCompDes.getParent());
			obj = exprHasObj(dStmtListCompIf.getListCompExpr().getExpr());
		}
		
		if(obj != null)
			Code.store(obj);
		
		if(listCompDes.getParent() instanceof DStmtListCompIf)
			Code.putJump(0);
		gotoCondAddr = Code.pc - 2;
		condTrueAddr = Code.pc;
		
		
		Code.loadConst(1);
		Code.put(Code.add);
		
		Code.putJump(exprAddr);
	}
	
	@Override
	public void visit(DStmtListComp dStmtListComp) {
		Code.fixup(exitAddr1);
		Code.fixup(exitAddr);
		Code.put(Code.pop);
		Code.put(Code.pop);
	}
	
	@Override
	public void visit(DStmtListCompIf dStmtListComp) {
		Code.fixup(exitAddr1);
		Code.fixup(exitAddr);
		Code.put(Code.pop);
		Code.put(Code.pop);
	}
	
	
//****************************************************************************************************************
//	Expr
//****************************************************************************************************************	
	
	@Override
	public void visit(Expr expr) {
		if(expr.getAddTerm() instanceof AddTermTerm) {
			AddTermTerm addTermTerm = (AddTermTerm) expr.getAddTerm();
			while(addTermTerm.getAddTerm() instanceof AddTermTerm) {
				addTermTerm = (AddTermTerm) addTermTerm.getAddTerm();
			}
			if(addTermTerm.getAddop() instanceof AddopPlus)
				Code.put(Code.add);
			else
				Code.put(Code.sub);
		}
	}
	
//****************************************************************************************************************
//	Term
//****************************************************************************************************************	
	
	@Override
	public void visit(AddTermTerm addTerm) {
		if(addTerm.getAddTerm() instanceof AddTermTerm) {
			if(addTerm.getAddop() instanceof AddopPlus)
				Code.put(Code.add);
			else
				Code.put(Code.sub);
		} 
	}
	
	@Override
	public void visit(Term term) {
		if(term.getMulFactor() instanceof MulFactorFactor) {
			MulFactorFactor mulFactorFactor = (MulFactorFactor) term.getMulFactor();
			if(mulFactorFactor.getMulop() instanceof MulopMul)
				Code.put(Code.mul);
			else if(mulFactorFactor.getMulop() instanceof MulopDiv)
				Code.put(Code.div);
			else
				Code.put(Code.rem);
		}
	}
	
//****************************************************************************************************************
//	Factor
//****************************************************************************************************************
	
	@Override
	public void visit(FactorOpDesignator factorOpDesignator) {
		Code.load(factorOpDesignator.getDesignator().obj);
	}
	
	@Override
	public void visit(MulFactorFactor mulFactor) {
		if(mulFactor.getMulFactor() instanceof MulFactorFactor) {
			if(mulFactor.getMulop() instanceof MulopMul)
				Code.put(Code.mul);
			else if(mulFactor.getMulop() instanceof MulopDiv)
				Code.put(Code.div);
			else
				Code.put(Code.rem);
		}
	}
	
	@Override
	public void visit(Factor factor) {
//		In factor, we must put minus if necessary
		if(factor.getFactorSign() instanceof FactorSignMinus) {
			Code.loadConst(-1);
			Code.put(Code.mul);
		}
	}
	
	@Override
	public void visit(FactorOpNumber factorNumber) {
		Code.loadConst(factorNumber.getFactVal());
	}
	
	@Override
	public void visit(FactorOpChar factorChar) {
		Code.loadConst(factorChar.getFactVal());
	}
	
	@Override
	public void visit(FactorOpBool factorBool) {
		Code.loadConst(factorBool.getFactVal());
	}
	
	@Override
	public void visit(FactorOpNew factorNew) {
		Code.put(Code.newarray);
		if(factorNew.getType().struct.equals(Tab.intType))
			Code.put(1);
		else
			Code.put(0);
	}
	
	@Override
	public void visit(FactorOpRange factorRange) {
		Code.put(Code.newarray);
		Code.put(1);
		Code.put(Code.dup);
		Code.put(Code.dup);
		Code.put(Code.arraylength);
//		here, i will have | arrAddr arrAddr arrLen
//		Save the pc for the loop
		int curr_pc = Code.pc;
		Code.loadConst(1);
		Code.put(Code.sub);
//		Decrement index, so we wont go out of bounds
		Code.put(Code.dup2);
		Code.put(Code.dup);
//		here i will have | arrAddr arrAddr arrIndex arrIndex
//		Do store 
		Code.put(Code.astore);
//		Duplicate value
		Code.put(Code.dup);
		Code.loadConst(0);
//		If not zero, repeat loop
		Code.putFalseJump(Code.eq, curr_pc);
//		If zero duplicate last index and do store arr[0] = [0]
		Code.put(Code.dup);
		Code.put(Code.astore);
	}
	
	
//****************************************************************************************************************
//	Standard methods
//****************************************************************************************************************
		
	private void put_enter(int b1, int b2) {
		Code.put(Code.enter);
		Code.put(b1);
		Code.put(b2);
	}
	
	private void put_return() {
		Code.put(Code.exit);
		Code.put(Code.return_);
	}
	
	@Override
	public void visit(ProgramName programName) {
		Obj obj = Tab.find("ord");
		obj.setAdr(Code.pc);
		put_enter(obj.getLevel(),  obj.getLocalSymbols().size());
		Code.put(Code.load);
		Code.put(0);
		put_return();
		obj = Tab.find("chr");
		obj.setAdr(Code.pc);
		put_enter(obj.getLevel(),  obj.getLocalSymbols().size());
		Code.put(Code.load);
		Code.put(0);
		put_return();
		obj = Tab.find("len");
		obj.setAdr(Code.pc);
		put_enter(obj.getLevel(),  obj.getLocalSymbols().size());
		Code.put(Code.load);
		Code.put(0);
		Code.put(Code.arraylength);
		put_return();
	}
	

		
//****************************************************************************************************************
//	Methods
//****************************************************************************************************************
	
	private Obj calledMethod = Tab.noObj;
	private Stack<Obj> methodStack = new Stack<>();
	
	@Override
	public void visit(DStmtDesMeth dStmtDesMeth) {
		if(methodStack.empty())
			calledMethod = Tab.noObj;
		else {
			calledMethod = methodStack.pop();
		}
	}
	
	@Override
	public void visit(FactorOpMethCall factorOpMethCall) {
		if(methodStack.empty())
			calledMethod = Tab.noObj;
		else {
			calledMethod = methodStack.pop();
		}
	}	
	
	@Override
	public void visit(ActParsExpr actParsExpr) {
		Collection<Obj> localsList =  calledMethod.getLocalSymbols();
		ArrayList<Obj> objArr = new ArrayList<>();
		for (Obj obj : localsList) {
			if(obj.getFpPos() == 1)
				objArr.add(obj);
		}
		for(int i = 0; i < objArr.size(); i++)
			Code.store(objArr.get(i));
		for(int i = 0; i < objArr.size(); i++)
			Code.load(objArr.get(i));
		int adr = calledMethod.getAdr();
		Code.put(Code.call);
		Code.put2(adr - (Code.pc - 1));
	}
	
	@Override
	public void visit(ActParsEpsilon actParsEpsilon) {
		int adr = calledMethod.getAdr();
		Code.put(Code.call);
		Code.put2(adr - (Code.pc - 1));
	}
	
	@Override
	public void visit(MethodCallName methodCallName) {
		if(!calledMethod.equals(Tab.noObj)) {
			methodStack.push(calledMethod);
		}
		calledMethod = methodCallName.getDesignator().obj;
	}

	@Override
	public void visit(StmtRet stmtRet) {
//		Should put data for B
		Code.put(Code.exit);
		Code.put(Code.return_);
	}
	
	@Override
	public void visit(MethodName methodName) {
//		Remember the address for B
		methodName.obj.setAdr(Code.pc);
//		Must have 3 bytes: enter b1 b2[formal, formal + local]
		Code.put(Code.enter);
//		level field -> number of formal parameters
		Code.put(methodName.obj.getLevel());
//		Whole locals
		Code.put(methodName.obj.getLocalSymbols().size());
		
		if(methodName.getMName().equals("main"))
			start_addr = methodName.obj.getAdr();
	}
	
	
	@Override
	public void visit(MethodDecl methodDecl) {
		Code.put(Code.exit);
		Code.put(Code.return_);
	}
	

	
//****************************************************************************************************************
//	Jumps
//****************************************************************************************************************
	
//	List<Map.Entry<String, Integer>> labList = new ArrayList<>();
//	List<Map.Entry<String, List<Integer>>> patchList = new ArrayList<>();
//	
//	@Override
//	public void visit(Label label) {
//		labList.add(new AbstractMap.SimpleEntry<>(label.getLName(), Code.pc));
//		if(patchList.contains(label.getLName())) {
//			List<Integer> patchFixIntegers = patchList.get(patchList.indexOf(label.getLName())).getValue();
//			for (Integer patchaAddr : patchFixIntegers) {
//				Code.fixup(patchaAddr);
//			}
//			patchList.remove(patchList.indexOf(label.getLName()));
//		}
//	}
//	
//	@Override
//	public void visit(StmtGoto stmtGoto) {
////		Will not-label terminal so there would be no insertion in labels map
//		if(labList.contains(stmtGoto.getLName())) {
//			Code.putJump(labList.get(labList.indexOf(stmtGoto.getLName())));
//		} else {
////			jmp 0 0
//			Code.putJump(0);
////			pc is after last zero
//			int patchAddr = Code.pc - 2;
//			if(patchList.contains(stmtGoto.getLName())) {
//				patchList.get(patchList.indexOf(stmtGoto.getLName())).getValue().add(patchAddr);
//			} else {
//				patchList.add(new AbstractMap.SimpleEntry<>(stmtGoto.getLName(), new ArrayList<Integer>()));
//				patchList.get(patchList.indexOf(stmtGoto.getLName())).getValue().add(patchAddr);
//			}
//		}
//	}
	
}
