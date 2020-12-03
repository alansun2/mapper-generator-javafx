package com.alan344.service.datasourcedriver;

import com.alan344.constants.DriveEnum;

import javax.sql.DataSource;

/**
 * @author AlanSun
 * @date 2020/9/30 16:15
 */
public class DatasourceDriverContext implements DatasourceDriver {
    private DatasourceDriver datasourceDriver;

    public DatasourceDriverContext(DriveEnum driveEnum) {
        if (driveEnum == DriveEnum.MYSQL_8_0_16) {
            datasourceDriver = MysqlDriver.getInstance();
        } else {
            datasourceDriver = OracleDriver.getInstance();
        }
    }


    /**
     * 创建数据源
     */
    @Override
    public DataSource createDataSource(com.alan344.bean.DataSource dataSource) {
        return datasourceDriver.createDataSource(dataSource);
    }

    /**
     * 获取数据源驱动名称
     */
    @Override
    public String getDrive(com.alan344.bean.DataSource dataSource) {
        return datasourceDriver.getDrive(dataSource);
    }
}
