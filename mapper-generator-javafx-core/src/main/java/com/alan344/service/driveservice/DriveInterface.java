package com.alan344.service.driveservice;

import com.alan344.bean.Column;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;

import java.util.List;

/**
 * @author AlanSun
 * @date 2020/12/2 16:06
 */
public interface DriveInterface {
    /**
     * 获取表
     *
     * @param dataSource 数据源
     * @return 表
     */
    List<Table> getTables(DataSource dataSource);

    /**
     * 获取某一表的所有字段
     *
     * @param dataSource 数据源
     * @param tableName  表
     * @return 表字段
     */
    List<Column> getColumn(DataSource dataSource, String tableName);
}
