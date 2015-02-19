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
        Term t1 = etaConversion();
        if(this!=t1)
            return t1.reduce();
        else
            return this;
    }

    @Override
    public String toString() {
        return "\\"+var.toString()+'.'+'('+term.toString()+')';
    }

    @Override
    public Term substitution(Variable variable, Term term) {
        if(variable.equals(var)){
            return this;
        }else{
            if(!term.getFreeVars().contains(variable)){
                return new Lambda(var,this.term.substitution(variable,term));
            }
            else {
                return renameNonFree(var, ReduceUtil.findName(var, term.getFreeVars())).substitution(variable,term);
            }
        }
    }
    public Term betaConversion(Term term){
        return this.term.substitution(var,term);


    }
    public Term etaConversion(){
        if(!term.getFreeVars().contains(var)){
            return term;
        }else{
            return this;
        }

    }

    @Override
    public Set<Variable> getFreeVars() {
        Set<Variable> result = term.getFreeVars();
        result.remove(var);
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
