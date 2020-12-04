package com.alan344.service.driveservice;

import com.alan344.constants.DriveEnum;
import com.alan344happyframework.exception.BizException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author AlanSun
 * @date 2020/12/2 16:56
 */
@Component
public class DriveFactory {

    @Autowired
    private MysqlDrive mysqlDrive;

    @Autowired
    private OracleDrive oracleDrive;

    public DriveInterface getDrive(DriveEnum driveEnum) {
        switch (driveEnum) {
            case ORACLE_11:
                return oracleDrive;
            case MYSQL_8_0_16:
                return mysqlDrive;
            default:
                throw new BizException("不支持该驱动");
        }
    }
}
