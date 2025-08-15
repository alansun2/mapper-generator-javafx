package com.alan344.bean;

import com.alan344.constants.enums.DriverEnum;
import com.alan344.utils.StringUtils;
import com.zaxxer.hikari.HikariDataSource;
import javafx.beans.property.SimpleStringProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Properties;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 21:33
 */
public class DataSource implements DataItem {
    private final SimpleStringProperty configName = new SimpleStringProperty();
    private final SimpleStringProperty url = new SimpleStringProperty();
    private final SimpleStringProperty driveName = new SimpleStringProperty();
    private final SimpleStringProperty user = new SimpleStringProperty();
    private final SimpleStringProperty password = new SimpleStringProperty();
    @Getter
    @Setter
    private Long sort;
    @Getter
    @Setter
    private transient List<Table> tables;

    @Setter
    private transient javax.sql.DataSource dataSource;

    public javax.sql.DataSource getDataSource() {
        if (dataSource != null) {
            return dataSource;
        }
        HikariDataSource hikariDataSource = new HikariDataSource();
        hikariDataSource.setUsername(this.getUser());
        hikariDataSource.setPassword(this.getPassword());
        hikariDataSource.setJdbcUrl(this.getUrl());
        hikariDataSource.setDriverClassName(this.getDriveName());
        Properties props = new Properties();
        // 设置可以获取remarks信息
        props.setProperty("remarks", "true");
        // 设置可以获取tables remarks信息
        props.setProperty("useInformationSchema", "true");
        hikariDataSource.setDataSourceProperties(props);
        dataSource = hikariDataSource;
        return hikariDataSource;
    }

    public void closeDataSource() {
        if (dataSource != null) {
            ((HikariDataSource) dataSource).close();
        }
    }

    public boolean isSame(DataSource dataSource) {
        return this.getConfigName().equals(dataSource.getConfigName()) && this.getUrl().equals(dataSource.getUrl())
                && this.getUser().equals(dataSource.getUser()) && this.getPassword().equals(dataSource.getPassword())
                && this.getDriveName().equals(dataSource.getDriveName());

    }

    public String getScheme() {
        final DriverEnum driver = this.getDriver();
        if (driver.equals(DriverEnum.MYSQL_8)) {
            String url = this.getUrl();
            final int startIndex = url.indexOf("/", 13);
            if (url.contains("?")) {
                final int endIndex = url.indexOf("?", 13);
                return url.substring(startIndex + 1, endIndex);
            } else {
                return url.substring(startIndex + 1);
            }
        } else if (driver.equals(DriverEnum.ORACLE_11)) {
            return this.getUser();
        } else if (driver.equals(DriverEnum.POSTGRESQL)) {
            // jdbc:postgresql://localhost:5432/test?currentSchema=test
            String url = this.getUrl();
            // get currentSchema from url for postgresql
            final int startIndex = url.indexOf("currentSchema", 14);
            final int endIndex = url.indexOf("&", startIndex);
            return url.substring(startIndex, endIndex == -1 ? url.length() : endIndex);
        }
        return null;
    }

    public DriverEnum getDriver() {
        final String lo = this.getUrl().toLowerCase();
        if (lo.contains("mysql")) {
            return DriverEnum.MYSQL_8;
        } else if (lo.contains("oracle")) {
            return DriverEnum.ORACLE_11;
        } else if (lo.contains("postgresql")) {
            return DriverEnum.POSTGRESQL;
        }
        return null;
    }

    @Override
    public String toString() {
        if (StringUtils.isNotEmpty(this.getConfigName())) {
            return this.getConfigName();
        }

        return "空";
    }

    public String getConfigName() {
        return configName.get();
    }

    public SimpleStringProperty configNameProperty() {
        return configName;
    }

    public void setConfigName(String configName) {
        this.configName.set(configName);
    }

    public String getUrl() {
        return url.get();
    }

    public SimpleStringProperty urlProperty() {
        return url;
    }

    public void setUrl(String url) {
        this.url.set(url);
    }

    public String getDriveName() {
        return driveName.get();
    }

    public SimpleStringProperty driveNameProperty() {
        return driveName;
    }

    public void setDriveName(String driveName) {
        this.driveName.set(driveName);
    }

    public String getUser() {
        return user.get();
    }

    public SimpleStringProperty userProperty() {
        return user;
    }

    public void setUser(String user) {
        this.user.set(user);
    }

    public String getPassword() {
        return password.get();
    }

    public SimpleStringProperty passwordProperty() {
        return password;
    }

    public void setPassword(String password) {
        this.password.set(password);
    }


    public DataSource copy() {
        DataSource dataSource = new DataSource();
        dataSource.setConfigName(this.getConfigName());
        dataSource.setUrl(this.getUrl());
        dataSource.setDriveName(this.getDriveName());
        dataSource.setUser(this.getUser());
        dataSource.setPassword(this.getPassword());
        return dataSource;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DataSource that = (DataSource) o;

        return configName.get().equals(that.configName.get());
    }

    @Override
    public int hashCode() {
        return configName.get().hashCode();
    }

    @Override
    public String getName() {
        return this.getConfigName();
    }
}
