package com.alan344.config;

import com.alibaba.fastjson2.JSONWriter;
import com.alibaba.fastjson2.writer.ObjectWriter;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;

import java.lang.reflect.Type;

/**
 * @author AlanSun
 * @date 2021/6/19 15:17
 **/
public class BooleanPropertyWriter implements ObjectWriter<SimpleBooleanProperty> {
    public static final BooleanPropertyWriter INSTANCE = new BooleanPropertyWriter();

    @Override
    public void write(JSONWriter jsonWriter, Object object, Object fieldName, Type fieldType, long features) {
        if (null == object) {
            jsonWriter.writeNull();
            return;
        }
        jsonWriter.writeBool(((BooleanProperty) object).get());
    }
}