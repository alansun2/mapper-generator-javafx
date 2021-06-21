package com.alan344.bean;

import com.zaxxer.hikari.HikariDataSource;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Objects;
import java.util.Properties;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 21:33
 */
@Getter
@Setter
public class DataSource implements DataItem {
    private String url;

    private String driveName;

    private String user;

    private String password;

    private String driveType1;

    private String host;

    private String port;
    /**
     * 数据库名称
     */
    private String database;

    private transient List<Table> tables;

    public javax.sql.DataSource createDataSource() {
        HikariDataSource hikariDataSource = new HikariDataSource();
        hikariDataSource.setUsername(this.user);
        hikariDataSource.setPassword(this.password);
        hikariDataSource.setJdbcUrl(this.url);
        hikariDataSource.setDriverClassName(this.driveName);
        Properties props = new Properties();
        // 设置可以获取remarks信息
        props.setProperty("remarks", "true");
        // 设置可以获取tables remarks信息
        props.setProperty("useInformationSchema", "true");
        hikariDataSource.setDataSourceProperties(props);
        return hikariDataSource;
    }

    @Override
    public String toString() {
        if (this.host == null) {
            return "空";
        } else {
            return this.host + "@" + this.database;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DataSource that = (DataSource) o;
        return Objects.equals(host, that.host) &&
                Objects.equals(database, that.database);
    }

    @Override
    public int hashCode() {
        return Objects.hash(host, database);
    }
}
