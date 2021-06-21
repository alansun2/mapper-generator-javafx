package com.alan344.bean;

import com.alan344.config.BooleanPropertySerializer;
import com.alibaba.fastjson.annotation.JSONField;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import lombok.Getter;
import lombok.Setter;

/**
 * @author AlanSun
 * @date 2019/8/12 13:43
 */
@Getter
@Setter
public class Column {
    private String columnName;
    private String type;
    private Integer size;
    private String remark;
    private boolean nullable;
    private boolean isAutoIncr;

    @JSONField(serializeUsing = BooleanPropertySerializer.class)
    private BooleanProperty ignore = new SimpleBooleanProperty(false);

    private ColumnOverride columnOverride = new ColumnOverride();

    /**
     * 用于业务的分页查询
     */
    private String condition;

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
