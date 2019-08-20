package com.alan344.constants;

import com.alan344.bean.DataSource;
import com.alan344.bean.Table;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 21:43
 */
public class BaseConstants {
    private static final String MG_HOME = System.getProperty("user.home") + "/AppData/Local/MapperGenerator";
    public static final String MG_DATA_HOME = MG_HOME + "/data/";
    private static final String MG_CONFIG_FILE = MG_HOME + "/config/base-config";

    /**
     * 选中的数据源
     */
    public static DataSource selectedDateSource;

    /**
     * 选中的要导出的表
     */
    public static Map<String, Table> selectedTableNameTableMap;

    /**
     * 记录该该表中的字段是否有过重写，如果有会在关闭应用时替换相应的表文件
     */
    public static Map<String, Boolean> tableNameIsOverrideRecodeMap = new HashMap<>();

    /**
     * 记录该该表中的字段是否有过重写，如果有会在关闭应用时替换相应的表文件
     */
    public static Map<String, Boolean> tableNameIsTableRecordMap = new HashMap<>();

    public static File getColumnsFile(DataSource dataSource, String tableName) {
        return new File(MG_DATA_HOME + dataSource.toString() + "_column/" + tableName);
    }

    public static File getColumnsDirectory(DataSource dataSource) {
        return new File(MG_DATA_HOME + dataSource.toString() + "_column");
    }

    public static File getTableFile(DataSource dataSource, String tableName) {
        return new File(MG_DATA_HOME + dataSource.toString() + "_table/" + tableName);
    }

    public static File getTableDirectory(DataSource dataSource) {
        return new File(MG_DATA_HOME + dataSource.toString() + "_table");
    }

    public static File getDataSourceFile(DataSource dataSource) {
        return new File(MG_DATA_HOME + dataSource.toString() + "_datasource");
    }

    public static File getConfigFile() {
        return new File(MG_CONFIG_FILE);
    }
}
