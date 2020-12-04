package com.alan344.service.datasourcedriver;

import com.alan344.bean.DataSource;
import com.zaxxer.hikari.HikariDataSource;

/**
 * @author AlanSun
 * @date 2020/9/30 10:55
 */
public class MysqlDriver implements DatasourceDriver {
    private MysqlDriver() {
        // nothing todo
    }

    private static final MysqlDriver MYSQLD_RIVER = new MysqlDriver();

    public static MysqlDriver getInstance() {
        return MYSQLD_RIVER;
    }

    /**
     * 创建数据源
     */
    @Override
    public javax.sql.DataSource createDataSource(DataSource dataSource) {
        HikariDataSource hikariDataSource = new HikariDataSource();
        hikariDataSource.setUsername(dataSource.getUser());
        hikariDataSource.setPassword(dataSource.getPassword());
        hikariDataSource.setJdbcUrl("jdbc:mysql://" + dataSource.getHost() + ":" + dataSource.getPort() + "/" + dataSource.getDatabase() + "?useUnicode=true&characterEncoding=utf-8&autoReconnect=true&useSSL=false&serverTimezone=CTT");
        hikariDataSource.setDriverClassName(dataSource.getDriveName());
        return hikariDataSource;
    }

    /**
     * 获取数据源驱动名称
     */
    @Override
    public String getDrive(DataSource dataSource) {
        return "jdbc:mysql://" + dataSource.getHost() + ":" + dataSource.getPort() + "/" + dataSource.getDatabase() + "?useUnicode=true&characterEncoding=utf-8&autoReconnect=true&useSSL=false&serverTimezone=CTT";
    }
}
