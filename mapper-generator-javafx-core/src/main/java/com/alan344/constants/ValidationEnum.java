package com.alan344.constants;

/**
 * @author AlanSun
 * @date 2020/10/25 17:14
 **/
public enum ValidationEnum {
    /**
     * 不为空
     */
    NOT_BLANK(),
    /**
     * NotNull
     */
    NotNull(),
    LENGTH(),
    Range(),
    
    ;

    private String className;
}
