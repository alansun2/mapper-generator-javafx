package com.alan344.bean;

import com.alan344.constants.DriverEnum;
import com.alan344.utils.StringUtils;
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
    private String configName;
    private String url;
    private String driveName;
    private String user;
    private String password;

    private transient List<Table> tables;

    private transient javax.sql.DataSource dataSource;

    public javax.sql.DataSource getDataSource() {
        if (dataSource != null) {
            return dataSource;
        }
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
        dataSource = hikariDataSource;
        return hikariDataSource;
    }

    public boolean isSame(DataSource dataSource) {
        return this.configName.equals(dataSource.getConfigName()) && this.url.equals(dataSource.getUrl())
                && this.user.equals(dataSource.getUser()) && this.password.equals(dataSource.getPassword())
                && this.driveName.equals(dataSource.getDriveName());

    }

    public String getScheme() {
        final DriverEnum driver = this.getDriver();
        if (driver.equals(DriverEnum.MYSQL_8_0_16)) {
            // jdbc:mysql://mysql_dev.tuoyang.vip:3306/school_safety
            final int startIndex = url.indexOf("/", 13);
            if (url.contains("?")) {
                final int endIndex = url.indexOf("?", 13);
                return url.substring(startIndex + 1, endIndex);
            } else {
                return url.substring(startIndex + 1);
            }
        } else if (driver.equals(DriverEnum.ORACLE_11)) {
            return user;
        }
        return null;
    }

    public DriverEnum getDriver() {
        final String lo = url.toLowerCase();
        if (lo.contains("mysql")) {
            return DriverEnum.MYSQL_8_0_16;
        } else if (lo.contains("oracle")) {
            return DriverEnum.ORACLE_11;
        }
        return null;
    }

    @Override
    public String toString() {
        if (StringUtils.isNotEmpty(configName)) {
            return configName;
        }

        return "空";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DataSource that = (DataSource) o;

        return Objects.equals(configName, that.configName);
    }

    @Override
    public int hashCode() {
        return configName != null ? configName.hashCode() : 0;
    }
}
