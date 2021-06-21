package com.alan344.config;

import com.alibaba.fastjson.serializer.JSONSerializer;
import com.alibaba.fastjson.serializer.ObjectSerializer;
import javafx.beans.property.BooleanProperty;

import java.lang.reflect.Type;

/**
 * @author AlanSun
 * @date 2021/6/19 15:17
 **/
public class BooleanPropertySerializer implements ObjectSerializer {

    @Override
    public void write(JSONSerializer serializer, Object object, Object fieldName, Type fieldType, int features) {
        BooleanProperty object1 = (BooleanProperty) object;
        serializer.write(object1.get());
    }
}