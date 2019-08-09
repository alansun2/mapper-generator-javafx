package com.alan344.bean;

import lombok.Getter;
import lombok.Setter;

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

    @Override
    public String toString() {
        if (this.host == null) {
            return "空";
        } else {
            return this.host + "@" + this.database;
        }
    }
}
