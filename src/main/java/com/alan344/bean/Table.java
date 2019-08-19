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

    private transient boolean returnInsertId;
    private transient boolean insert = true;
    private transient boolean count = true;
    private transient boolean update = true;
    private transient boolean delete = true;
    private transient boolean select = true;


    @Override
    public String toString() {
        return this.tableName;
    }
}
