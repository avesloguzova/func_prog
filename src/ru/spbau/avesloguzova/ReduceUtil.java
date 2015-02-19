package ru.spbau.avesloguzova;

import java.util.Set;

/**
 * Created by av on 2/19/15.
 */
public class ReduceUtil {
    public static Variable findName(Variable var, Set<Variable>variables){
        if(variables.contains(var))
            return findName(new Variable(var.getName() + '\''),variables);
        else
            return var;
    }
}
