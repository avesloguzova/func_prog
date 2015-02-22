package ru.spbau.avesloguzova;

import java.util.Set;

/**
 * Created by av on 2/18/15.
 */
public interface Term {
    public Term reduce();
    public Term substitution(Variable variable, Term term);
    public Set<Variable> getFreeVars();

    public Set<Variable> getNonFreeVars();
    public Term renameNonFree(Variable varFrom, Variable varTo);
    public Term forcedRenamed(Variable varFrom,Variable varTo);

}
