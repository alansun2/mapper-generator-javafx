package com.alan344.bean;

import com.alan344.config.BooleanPropertyWriter;
import com.alibaba.fastjson2.annotation.JSONField;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import lombok.Getter;
import lombok.Setter;

/**
 * @author AlanSun
 * @since 2019/8/12 13:43
 */
@Getter
@Setter
public class Column {
    private String columnName;
    private String type;
    private Integer size;
    private String remark;
    @JSONField(serializeUsing = BooleanPropertyWriter.class)
    private BooleanProperty nonNullable = new SimpleBooleanProperty(true);
    private boolean isAutoIncr;
    @JSONField(serializeUsing = BooleanPropertyWriter.class)
    private BooleanProperty ignore = new SimpleBooleanProperty(false);
    private ColumnOverride columnOverride = new ColumnOverride();

    /**
     * 用于业务的分页查询
     */
    private String condition;

    public void setNonNullable(boolean nonNullable) {
        this.nonNullable.set(nonNullable);
    }

    public BooleanProperty nonNullableProperty() {
        return nonNullable;
    }

    public boolean isNonNullable() {
        return nonNullable.get();
    }

    public boolean isIgnore() {
        return ignore.get();
    }

    public BooleanProperty ignoreProperty() {
        return ignore;
    }

    public void setIgnore(boolean ignore) {
        this.ignore.set(ignore);
    }
}
