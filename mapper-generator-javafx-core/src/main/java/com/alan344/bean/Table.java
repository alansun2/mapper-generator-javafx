package com.alan344.bean;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Objects;

/**
 * @author AlanSun
 * @date 2019/8/9 14:07
 */
@Getter
@Setter
public class Table implements DataItem {
    private String tableName;

    private transient List<Column> columns;

    private boolean returnInsertId;
    private boolean insert = true;
    private boolean count = true;
    private boolean update = true;
    private boolean delete = true;
    private boolean select = true;
    private boolean updateExample = true;
    private boolean deleteExample = true;
    private boolean selectExample = true;


    @Override
    public String toString() {
        return this.tableName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Table table = (Table) o;
        return Objects.equals(tableName, table.tableName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(tableName);
    }
}
