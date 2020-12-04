package com.alan344.constants;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import org.mybatis.generator.my.config.MybatisExportConfig;
import javafx.collections.ObservableList;
import javafx.scene.control.TreeItem;
import javafx.scene.layout.VBox;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
     * all dataSource
     */
    public static Map<TreeItem<DataItem>, DataSource> allDataSources = new HashMap<>(8);

    /**
     * 点击导出后，选中的数据源
     */
    public static DataSource selectedDateSource;

    /**
     * 选中的要导出的表
     */
    public static Map<String, Table> selectedTableNameTableMap;

    /**
     * 用于当再不同的 dataSource 之间切换时，保留原来的 VBox
     */
    public static Map<DataSource, ObservableList<VBox>> dataSourceTableVBoxListMap = new HashMap<>();

    /**
     * 用于当再不同的 dataSource 之间切换时，保留原来的 tables
     */
    public static Map<DataSource, Map<String, Table>> dataSourceTableListMap = new HashMap<>();

    /**
     * 记录该该表中的字段是否有过重写，如果有会在关闭应用时替换相应的 column 文件
     */
    public static Map<String, Boolean> tableNameIsOverrideRecodeMap = new HashMap<>();

    /**
     * 记录该该表中的设置（如 选择insert等）是否有过改过，如果有会在关闭应用时替换相应的table文件
     */
    public static Map<String, Boolean> tableNameSetUpTableRecordMap = new HashMap<>();

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

    /**
     * 获取配置信息目录
     *
     * @return 配置信息目录
     */
    public static File getConfigFile() {
        return new File(MG_CONFIG_FILE);
    }

    public static List<VBox> selectedCheckBoxVBox = new ArrayList<>();

    public static MybatisExportConfig curMybatisExportConfig;
}
