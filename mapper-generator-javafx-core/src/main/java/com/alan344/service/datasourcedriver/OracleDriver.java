package com.alan344.service.datasourcedriver;

import com.zaxxer.hikari.HikariDataSource;

import javax.sql.DataSource;

/**
 * @author AlanSun
 * @date 2020/9/30 10:59
 */
public class OracleDriver implements DatasourceDriver {
    private OracleDriver() {
        // nothing to do
    }

    private static final OracleDriver ORACLE_DRIVER = new OracleDriver();

    public static OracleDriver getInstance() {
        return ORACLE_DRIVER;
    }

    /**
     * 创建数据源
     */
    @Override
    public DataSource createDataSource(com.alan344.bean.DataSource dataSource) {
        HikariDataSource hikariDataSource = new HikariDataSource();
        hikariDataSource.setUsername(dataSource.getUser());
        hikariDataSource.setPassword(dataSource.getPassword());
        hikariDataSource.setJdbcUrl("jdbc:oracle:thin:@" + dataSource.getHost() + ":" + dataSource.getPort() + ":" + dataSource.getServiceName());
        hikariDataSource.setDriverClassName(dataSource.getDriveName());
        return hikariDataSource;
    }

    /**
     * 获取数据源驱动名称
     */
    @Override
    public String getDrive(com.alan344.bean.DataSource dataSource) {
        return "jdbc:oracle:thin:@" + dataSource.getHost() + ":" + dataSource.getPort() + ":" + dataSource.getServiceName();
    }
}
