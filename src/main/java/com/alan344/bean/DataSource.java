package com.alan344.bean;

import lombok.Getter;
import lombok.Setter;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 21:33
 */
@Getter
@Setter
public class DataSource {

    private String host;

    private String port;

    private String database;

    private String user;

    private String password;
}
