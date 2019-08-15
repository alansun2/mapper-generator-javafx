package com.alan344.constants;

import com.alan344.bean.DataSource;
import com.alan344.bean.Table;

import java.io.File;
import java.util.List;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 21:43
 */
public class BaseConstants {
    private static final String MG_HOME = System.getProperty("user.home") + "/AppData/Local/MapperGenerator";
    public static final String MG_DATA_HOME = MG_HOME + "/data/";
    public static final String MG_CONFIG_HOME = MG_HOME + "/config/";

    /**
     * 选中的数据源
     */
    public static DataSource currentDateSource;

    /**
     * 选中的要导出的表
     */
    public static List<Table> selectedTables;

    public static File getColumnsFile(DataSource dataSource) {
        return new File(MG_DATA_HOME + dataSource.toString() + "_columns");
    }

    public static File getTableFile(DataSource dataSource) {
        return new File(MG_DATA_HOME + dataSource.toString() + "_table");
    }

    public static File getDataSourceFile(DataSource dataSource) {
        return new File(MG_DATA_HOME + dataSource.toString() + "_datasource");
    }
}
