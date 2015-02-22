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
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Application)) return false;

        Application that = (Application) o;

        if (!first.equals(that.first)) return false;
        return secound.equals(that.secound);

    }

    @Override
    public int hashCode() {
        int result = first.hashCode();
        result = 31 * result + secound.hashCode();
        return result;
    }

    @Override
    public Term reduce() {
        Term result;
        if(first instanceof Lambda){
            result = ((Lambda) first).betaConversion(secound).reduce();
        }else{
            Term t1 = first.reduce();
            Term t2 = secound.reduce();
            if (first.equals(t1))
                result = new Application(t1, t2);
            else
                result = new Application(t1, t2).reduce();
        }
        return result;
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
    public Set<Variable> getNonFreeVars() {
        Set<Variable> set1 = first.getNonFreeVars();
        set1.addAll(secound.getNonFreeVars());
        return set1;
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
