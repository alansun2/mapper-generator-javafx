package com.alan344.convert;

import com.alan344.constants.ConditionEnum;
import javafx.util.StringConverter;

/**
 * @author AlanSun
 * @date 2020/9/9 17:24
 */
public class ConditionEnumConvert extends StringConverter<ConditionEnum> {

    @Override
    public String toString(ConditionEnum object) {
        if (object != null) {
            return object.getSymbol();
        }
        return null;
    }


    @Override
    public ConditionEnum fromString(String string) {
        return ConditionEnum.valueOf(string);
    }
}
