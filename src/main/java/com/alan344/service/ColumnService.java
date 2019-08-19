package com.alan344.service;

import com.alan344.bean.Column;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alibaba.fastjson.JSONArray;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2019/8/15 12:10
 */
@Slf4j
@Service
public class ColumnService {
    @Autowired
    private BeanFactory beanFactory;

    /**
     * 获取表的字段
     *
     * @param tableName tableName
     * @return 字段数组
     */
    public List<Column> getColumnsFromRemote(DataSource dataSource, String tableName) {
        JdbcTemplate jdbcTemplate = beanFactory.getBean(dataSource.toString(), JdbcTemplate.class);
        List<Column> columns = new ArrayList<>();
        jdbcTemplate.query("DESC " + tableName, (rs, rowNum) -> {
            Column column = new Column();
            column.setColumnName(rs.getString(1));
            column.setType(rs.getString(2));
            columns.add(column);
            return column;
        });

        return columns;
    }

    /**
     * 重新加载表字段信息
     *
     * @param tableName 表命
     */
    public void refreshColumns(String tableName) {
        Table table = BaseConstants.selectedTableNameTableMap.get(tableName);
        List<Column> existColumns = table.getColumns();
        Map<String, Column> existColumnNameColumnMap = existColumns.stream().collect(Collectors.toMap(Column::getColumnName, column -> column));

        DataSource dataSource = BaseConstants.selectedDateSource;
        List<Column> columns = this.getColumnsFromRemote(dataSource, tableName);
        deleteColumnFile(dataSource, tableName);

        for (Column column : columns) {
            if (existColumnNameColumnMap.containsKey(column.getColumnName())) {
                column.setIgnore(existColumnNameColumnMap.get(column.getColumnName()).isIgnore());
            }
        }

        table.setColumns(columns);
    }

    /**
     * 加载columns
     *
     * @param dataSource 数据源
     * @param tables     表
     */
    void loadColumns(DataSource dataSource, List<Table> tables) {
        Map<String, List<Column>> tableNameColumnsMap = new HashMap<>();
        for (Table table : tables) {
            String tableName = table.getTableName();
            List<Column> columns = this.getColumnsFromRemote(dataSource, tableName);
            table.setColumns(columns);
            tableNameColumnsMap.put(tableName, columns);
        }

        //写入文件
        this.downLoadColumnsToFile(dataSource, tableNameColumnsMap);
    }

    /**
     * columns 写入文件
     *
     * @param dataSource          数据源
     * @param tableNameColumnsMap columnsMap
     */
    @Async
    void downLoadColumnsToFile(DataSource dataSource, Map<String, List<Column>> tableNameColumnsMap) {
        tableNameColumnsMap.forEach((tableName, columns) -> {
            File columnsFile = BaseConstants.getColumnsFile(dataSource, tableName);
            String tableNameColumnsMapStr = JSONArray.toJSONString(columns, true);
            try {
                FileUtils.writeStringToFile(columnsFile, tableNameColumnsMapStr);
            } catch (IOException e) {
                log.error("columns download 失败");
            }
        });
    }

    /**
     * 从文件加载字段信息
     *
     * @param dataSource 数据源
     */
    void loadColumnsFromFile(DataSource dataSource, List<Table> tables) {
        File columnsDirectory = BaseConstants.getColumnsDirectory(dataSource);
        if (!columnsDirectory.exists()) {
            return;
        }

        File[] columnsFiles = columnsDirectory.listFiles();
        if (columnsFiles == null || columnsFiles.length <= 0) {
            return;
        }

        Map<String, Table> tableNameTableMap = tables.stream().collect(Collectors.toMap(Table::getTableName, table -> table));

        for (File columnsFile : columnsFiles) {
            try {
                String tableNameColumns = FileUtils.readFileToString(columnsFile);

                List<Column> columns = JSONArray.parseArray(tableNameColumns, Column.class);
                Table table = tableNameTableMap.get(columnsFile.getName());
                if (table != null) {
                    table.setColumns(columns);
                }
            } catch (IOException e) {
                log.error("加载columns文件失败", e);
            }
        }
    }

    /**
     * 删除table
     *
     * @param dataSource 数据源信息
     */
    void deleteColumns(DataSource dataSource, String tableName) {
        this.deleteColumnFile(dataSource, tableName);
    }

    /**
     * 删除table
     *
     * @param dataSource 数据源信息
     */
    void deleteColumnsDirectory(DataSource dataSource) {
        this.deleteColumnDirectory(dataSource);
    }

    /**
     * 把columns文件从磁盘删除
     */
    @Async
    void deleteColumnFile(DataSource dataSource, String tableName) {
        try {
            FileUtils.forceDelete(BaseConstants.getColumnsFile(dataSource, tableName));
        } catch (IOException e) {
            log.error("删除字段文件错误", e);
        }
    }

    /**
     * 把columns文件从磁盘删除
     */
    @Async
    void deleteColumnDirectory(DataSource dataSource) {
        try {
            FileUtils.forceDelete(BaseConstants.getColumnsDirectory(dataSource));
        } catch (IOException e) {
            log.error("删除字段文件错误", e);
        }
    }
}
