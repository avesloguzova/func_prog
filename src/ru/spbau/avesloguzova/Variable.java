package ru.spbau.avesloguzova;

import java.util.HashSet;
import java.util.Set;

/**
 * Created by av on 2/18/15.
 */
public class Variable implements Term {
    private final String name;
    public Variable(String name){
        this.name = name;
    }

    @Override
    public Term reduce() {
        return this;
    }

    @Override
    public Term substitution(Variable variable, Term term) {
        if(this.equals(variable))
            return term;
        else
            return this;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public Set<Variable> getFreeVars() {
        HashSet<Variable> result = new HashSet<Variable>();
        result.add(this);
        return result;
    }

    @Override
    public Set<Variable> getNonFreeVars() {
        return new HashSet<Variable>();
    }

    @Override
    public Term renameNonFree(Variable varFrom, Variable varTo) {
       return this;
    }

    @Override
    public Term forcedRenamed(Variable varFrom, Variable varTo) {
        if(varFrom.equals(this)){
            return (varTo);
        }else{
            return this;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Variable variable = (Variable) o;

        return !(name != null ? !name.equals(variable.name) : variable.name != null);

    }

    @Override
    public int hashCode() {
        return name != null ? name.hashCode() : 0;
    }

    public String getName() {
        return name;
    }
}
