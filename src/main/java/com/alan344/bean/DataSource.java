package com.alan344.bean;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Objects;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 21:33
 */
@Getter
@Setter
public class DataSource implements DataItem {

    private String host;

    private String port;

    private String database;

    private String user;

    private String password;

    private String driveName;

    private transient List<Table> tables;

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
