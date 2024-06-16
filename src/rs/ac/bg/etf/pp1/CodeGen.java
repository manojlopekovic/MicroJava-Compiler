package rs.ac.bg.etf.pp1;

import java.awt.Label;
import java.security.KeyPair;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
// Statement
//****************************************************************************************************************	
	
	@Override
	public void visit(StmtRet stmtRet) {
//		Should put data for B
		Code.put(Code.exit);
		Code.put(Code.return_);
	}
	
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
				
				Code.put(Code.aload);
				Code.loadConst(0);
				if(stmtPrint.getExpr().struct.equals(Tab.charType))
					Code.put(Code.bprint);
				else 
					Code.put(Code.print);
				
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
//	Methods
//****************************************************************************************************************
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
