package ru.spbau.avesloguzova;

import java.util.Set;

/**
 * Created by av on 2/18/15.
 */
public class Lambda implements Term{

    private final Variable var;
    private final Term term;

    public Lambda(Variable var , Term term){
        this.term = term;
        this.var = var;
    }

    @Override
    public Term reduce() {
        return new Lambda(var, term.reduce());
    }

    @Override
    public String toString() {
        return '(' + "\\" + var.toString() + '.' + term.toString() + ')';
    }

    @Override
    public Term substitution(Variable variable, Term term) {
        Term result;
        Set<Variable> fv = term.getFreeVars();
        Set<Variable> nfv = this.getNonFreeVars();
        nfv.retainAll(fv);
        Lambda tmpTerm = this;
        for (Variable nfVar : nfv) {
            tmpTerm = (Lambda) renameNonFree(nfVar, ReduceUtil.findName(nfVar, fv));
        }

        result = new Lambda(tmpTerm.var, (tmpTerm).term.substitution(variable, term));
        return result;

    }
    public Term betaConversion(Term term){
        return this.term.substitution(var,term);
    }

    @Override
    public Set<Variable> getFreeVars() {
        Set<Variable> result = term.getFreeVars();
        result.remove(var);
        return result;
    }

    @Override
    public Set<Variable> getNonFreeVars() {
        Set<Variable> result = term.getNonFreeVars();
        result.add(var);
        return result;
    }

    @Override
    public Term renameNonFree(Variable varFrom, Variable varTo) {
        if(var.equals(varFrom)){
            return new Lambda(varTo, term.forcedRenamed(varFrom,varTo));
        }
        else{
            return new Lambda(var, term.renameNonFree(varFrom,varTo));
        }
    }

    @Override
    public Term forcedRenamed(Variable varFrom, Variable varTo) {
        if(var.equals(varFrom)){
            return new Lambda(varTo, term.forcedRenamed(varFrom,varTo));
        }
        else{
            return new Lambda(var, term.forcedRenamed(varFrom, varTo));
        }
    }
}
