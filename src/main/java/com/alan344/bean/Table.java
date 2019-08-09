package com.alan344.bean;

import lombok.Getter;
import lombok.Setter;

/**
 * @author AlanSun
 * @date 2019/8/9 14:07
 */
@Getter
@Setter
public class Table implements DataItem {
    private String tableName;

    @Override
    public String toString() {
        return this.tableName;
    }
}
