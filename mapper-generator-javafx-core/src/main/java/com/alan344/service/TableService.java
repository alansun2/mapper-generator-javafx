package com.alan344.service;

import cn.hutool.core.io.FileUtil;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.DataSourceUtils;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @since 2019/8/9 16:48
 */
@Slf4j
@Service
public class TableService {
    @Autowired
    private ColumnService columnService;

    /**
     * 刷新tables
     *
     * @param dataSource 数据源
     */
    public List<Table> refreshTables(DataSource dataSource) {
        List<Table> remoteTables = this.pullTablesFromRemote(dataSource);
        if (!remoteTables.isEmpty()) {
            List<Table> existTables = dataSource.getTables();
            if (existTables != null && !existTables.isEmpty()) {
                Map<String, Table> tableNameTableMap = existTables.stream().collect(Collectors.toMap(Table::getTableName,
                        table -> table));
                for (Table remoteTable : remoteTables) {
                    if (tableNameTableMap.containsKey(remoteTable.getTableName())) {
                        Table existTable = tableNameTableMap.get(remoteTable.getTableName());
                        if (null == existTable) {
                            continue;
                        }
                        remoteTable.setReturnInsertId(existTable.isReturnInsertId());
                        remoteTable.setInsert(existTable.isInsert());
                        remoteTable.setCount(existTable.isCount());
                        remoteTable.setUpdate(existTable.isUpdate());
                        remoteTable.setDelete(existTable.isDelete());
                        remoteTable.setSelect(existTable.isSelect());
                        remoteTable.setUpdateExample(existTable.isUpdateExample());
                        remoteTable.setDeleteExample(existTable.isDeleteExample());
                        remoteTable.setSelectExample(existTable.isSelectExample());
                    }
                }
            }

            // 删除表文件夹
            this.deleteTableDirectory(dataSource);
            // 表写入文件
            this.downLoadToFileBatch(dataSource, remoteTables);
        }
        return remoteTables;
    }

    /**
     * 从远程拉去表信息
     *
     * @param dataSource 数据源
     * @return {@link Table}
     */
    private List<Table> pullTablesFromRemote(DataSource dataSource) {
        final List<Table> tables;
        try (final Connection connection = dataSource.getDataSource().getConnection()) {
            tables = DataSourceUtils.getTables(connection);
        } catch (SQLException e) {
            log.error("获取连接异常", e);
            throw new RuntimeException("获取数据库连接异常");
        }
        dataSource.setTables(tables);
        return tables;
    }

    /**
     * 从文件加表信息至pane
     *
     * @param dataSource 数据源
     */
    public void loadTables(DataSource dataSource) {
        List<Table> tables = new ArrayList<>();

        File tableDirectory = BaseConstants.getTableDirectory(dataSource);
        if (!tableDirectory.exists()) {
            this.loadTablesFromRemote(dataSource);
        } else {
            File[] files = tableDirectory.listFiles();
            if (files == null || files.length == 0) {
                return;
            }

            try {
                for (File file : files) {
                    Table table = JSONObject.parseObject(FileUtils.readFileToString(file, StandardCharsets.UTF_8.toString()),
                            Table.class);
                    tables.add(table);
                }
            } catch (IOException e) {
                log.error("加载tables文件失败", e);
                return;
            }
            dataSource.setTables(tables);
            columnService.loadColumnsFromFile(dataSource, tables);
        }
    }

    /**
     * 当展开datasource时加载tableItem，并将table写入文件
     *
     * @param dataSource 数据源的item
     */
    private void loadTablesFromRemote(DataSource dataSource) {
        List<Table> tables = this.pullTablesFromRemote(dataSource);
        if (!tables.isEmpty()) {
            // 加载columns
            columnService.loadColumns(dataSource, tables);
            dataSource.setTables(tables);
            // 写入文件
            this.downLoadToFileBatch(dataSource, tables);
        }
    }

    /**
     * 删除table 同时删除columns
     *
     * @param dataSource 数据源信息
     */
    void deleteTables(DataSource dataSource) {
        this.deleteTableDirectory(dataSource);

        columnService.deleteColumnsAll(dataSource);
    }

    /**
     * 删除table 同时删除columns
     *
     * @param dataSource 数据源信息
     */
    void updateTableAndColumnName(DataSource old, DataSource dataSource) {
        if (FileUtil.exist(BaseConstants.getTableDirectory(old))) {
            FileUtil.rename(BaseConstants.getTableDirectory(old), BaseConstants.getTableDirectory(dataSource).getName(), true);
            FileUtil.rename(BaseConstants.getColumnsDirectory(old), BaseConstants.getColumnsDirectory(dataSource).getName(),
                    true);
        }
    }

    /**
     * 把tables信息记录到文件
     */
    @Async
    void downLoadToFileBatch(DataSource dataSource, List<Table> tables) {
        try {
            for (Table table : tables) {
                String tablesStr = JSON.toJSONString(table, JSONWriter.Feature.PrettyFormat);
                FileUtils.writeStringToFile(BaseConstants.getTableFile(dataSource, table.getTableName()), tablesStr,
                        StandardCharsets.UTF_8.toString());
            }
        } catch (IOException e) {
            log.error("写入表文件错误", e);
        }
    }

    /**
     * 把table信息记录到文件
     */
    @Async
    void downLoadToFileSingle(DataSource dataSource, Table table) {
        try {
            String tablesStr = JSON.toJSONString(table, JSONWriter.Feature.PrettyFormat);
            FileUtils.writeStringToFile(BaseConstants.getTableFile(dataSource, table.getTableName()), tablesStr,
                    StandardCharsets.UTF_8.toString());
        } catch (IOException e) {
            log.error("写入表文件错误", e);
        }
    }

    /**
     * 把 table 文件从磁盘删除
     */
    @Async
    void deleteTableFile(DataSource dataSource, String tableName) {
        try {
            FileUtils.forceDelete(BaseConstants.getTableFile(dataSource, tableName));
        } catch (IOException e) {
            log.error("删除表文件错误", e);
        }
    }

    /**
     * 把 table 目录从磁盘删除
     */
    @Async
    void deleteTableDirectory(DataSource dataSource) {
        try {
            FileUtils.forceDelete(BaseConstants.getTableDirectory(dataSource));
        } catch (IOException e) {
            log.error("删除表文件错误", e);
        }
    }

    /**
     * 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 table 配置（如 insert）文件重写
     */
    public void downLoadTableIfOverrideModify() {
        Map<String, Boolean> tableNameIsTableRecordMap = BaseConstants.tableNameSetUpTableRecordMap;
        if (!tableNameIsTableRecordMap.isEmpty()) {
            tableNameIsTableRecordMap.forEach((tableName, record) -> {
                this.deleteTableFile(BaseConstants.selectedDateSource, tableName);
                Table table = BaseConstants.selectedTableNameTableMap.get(tableName);
                this.downLoadToFileSingle(BaseConstants.selectedDateSource, table);
            });
            // 清空map,因为有多个数据源，一个导出结束后，用户可能还会选择别的数据源进行导出
            BaseConstants.tableNameSetUpTableRecordMap.clear();
        }
    }
}
