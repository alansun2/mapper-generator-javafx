package com.alan344.constants;

/**
 * @author AlanSun
 * @date 2020/9/30 10:43
 */
public enum DriverEnum {

    /**
     * mysql 8.0.16 版本
     */
    MYSQL_8_0_16("com.mysql.cj.jdbc.Driver", "3306"),
    /**
     * oracle 11g
     */
    ORACLE_11("oracle.jdbc.OracleDriver", "1521"),
    ;

    private final String drive;

    private final String defaultPort;

    public String getDrive() {
        return drive;
    }

    public String getDefaultPort() {
        return defaultPort;
    }

    DriverEnum(String drive, String defaultPort) {
        this.drive = drive;
        this.defaultPort = defaultPort;
    }
}
