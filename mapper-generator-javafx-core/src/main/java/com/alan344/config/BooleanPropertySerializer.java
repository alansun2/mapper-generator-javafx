package com.alan344.config;

import com.alibaba.fastjson.serializer.JSONSerializer;
import com.alibaba.fastjson.serializer.ObjectSerializer;
import javafx.beans.property.BooleanProperty;

import java.io.IOException;
import java.lang.reflect.Type;

public class BooleanPropertySerializer implements ObjectSerializer {

    @Override
    public void write(JSONSerializer serializer, Object object, Object fieldName, Type fieldType,
                      int features) throws IOException {
        BooleanProperty object1 = (BooleanProperty) object;
        serializer.write(object1.get());
    }
}