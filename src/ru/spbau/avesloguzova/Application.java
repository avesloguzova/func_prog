package ru.spbau.avesloguzova;

import java.util.HashSet;
import java.util.Set;

/**
 * Created by av on 2/18/15.
 */
public class Application implements Term{
    private final Term first;
    private final Term secound;
    public Application(Term term1,Term term2){
        first = term1;
        secound = term2;
    }

    @Override
    public Term reduce() {
        if(first instanceof Lambda){
            return ((Lambda)first).betaConversion(secound).reduce();
        }else{
            return new Application(first.reduce(),secound.reduce());
        }
    }

    @Override
    public String toString() {
        return "("+first.toString()+ "  "+secound.toString()+")";
    }

    @Override
    public Term substitution(Variable variable, Term term) {
        return new Application(first.substitution(variable,term),secound.substitution(variable,term));
    }

    @Override
    public Set<Variable> getFreeVars() {
        Set<Variable> result = new HashSet<Variable>();
        result.addAll(first.getFreeVars());
        result.addAll(secound.getFreeVars());
        return result;
    }

    @Override
    public Term renameNonFree(Variable varFrom, Variable varTo) {
        return new Application(first.renameNonFree(varFrom, varTo),secound.renameNonFree(varFrom, varTo));
    }

    @Override
    public Term forcedRenamed(Variable varFrom, Variable varTo) {
        return new Application(first.forcedRenamed(varFrom,varTo),secound.forcedRenamed(varFrom,varTo));
    }
}
