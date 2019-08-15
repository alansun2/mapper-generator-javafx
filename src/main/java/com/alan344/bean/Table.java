package com.alan344.bean;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

/**
 * @author AlanSun
 * @date 2019/8/9 14:07
 */
@Getter
@Setter
public class Table implements DataItem {
    private String tableName;

    private List<Column> columns;

    @Override
    public String toString() {
        return this.tableName;
    }
}
