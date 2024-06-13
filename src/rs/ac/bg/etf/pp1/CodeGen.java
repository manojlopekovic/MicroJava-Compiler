package rs.ac.bg.etf.pp1;

import rs.ac.bg.etf.pp1.ast.*;
import rs.etf.pp1.mj.runtime.Code;
import rs.etf.pp1.symboltable.Tab;

public class CodeGen extends VisitorAdaptor{
	
	private int mainPC = 0;

	public int mainPcVal() {
		return mainPC;
	}

	
//****************************************************************************************************************
//	Designator
//****************************************************************************************************************
	
	
	
//****************************************************************************************************************
//	Expr
//****************************************************************************************************************	
	
	@Override
	public void visit(Expr expr) {
		if(expr.getAddTerm() instanceof AddTermTerm) {
			AddTermTerm addTermTerm = (AddTermTerm) expr.getAddTerm();
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
	
	@Override
	public void visit(AddTermTerm addTerm) {
		if(addTerm.getAddTerm() instanceof AddTermTerm) {
			if(addTerm.getAddop() instanceof AddopPlus)
				Code.put(Code.add);
			else
				Code.put(Code.sub);
		}
	}
	
//****************************************************************************************************************
//	Factor
//****************************************************************************************************************
	
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
	
//****************************************************************************************************************
//	Print
//****************************************************************************************************************	
	
	@Override
	public void visit(StmtPrintExpr stmtPrint) {
		
//		Needs ..., val, width on exprStack
		Code.loadConst(0);
		if(stmtPrint.getExpr().struct.equals(Tab.charType))
			Code.put(Code.bprint);
		else 
			Code.put(Code.print);
	}
	
	@Override
	public void visit(StmtPrintNum stmtPrint) {
		
	}
	
//****************************************************************************************************************
//	Methods
//****************************************************************************************************************
	@Override
	public void visit(MethodName methodName) {
//		Must have 3 bytes: enter b1 b2[formal, formal + local]
		Code.put(Code.enter);
//		level field -> number of formal parameters
		Code.put(methodName.obj.getLevel());
//		Whole locals
		Code.put(methodName.obj.getLocalSymbols().size());
	}
	
	@Override
	public void visit(MethodDecl methodDecl) {
		Code.put(Code.exit);
		Code.put(Code.return_);
	}
	
}
