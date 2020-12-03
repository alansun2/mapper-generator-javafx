package com.alan344.service.datasourcedriver;

import com.alan344.bean.DataSource;

/**
 * @author Alan
 * @date 2020/9/30 10:54
 */
public interface DatasourceDriver {

    /**
     * 创建数据源
     */
    javax.sql.DataSource createDataSource(DataSource dataSource);

    /**
     * 获取数据源驱动名称
     */
    String getDrive(DataSource dataSource);
}
