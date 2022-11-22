package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import vip.tuoyang.base.core.bean.response.Page;
import vip.tuoyang.schoolsafe.basic.api<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}ServiceI;
import vip.tuoyang.schoolsafe.basic.dto.${TYPE_NAME_UPPER_CAMEL}Cmd;
import vip.tuoyang.schoolsafe.basic.dto.${TYPE_NAME_UPPER_CAMEL}PageQry;
import vip.tuoyang.schoolsafe.basic.dto.data.${TYPE_NAME_UPPER_CAMEL}DTO;
import vip.tuoyang.schoolsafe.basic<#if DOMAIN != "">.${DOMAIN}</#if>.executor.${TYPE_NAME_UPPER_CAMEL}AddCmdExe;
import vip.tuoyang.schoolsafe.basic<#if DOMAIN != "">.${DOMAIN}</#if>.executor.${TYPE_NAME_UPPER_CAMEL}DelByIdCmdExe;
import vip.tuoyang.schoolsafe.basic<#if DOMAIN != "">.${DOMAIN}</#if>.executor.${TYPE_NAME_UPPER_CAMEL}UpdateCmdExe;
import vip.tuoyang.schoolsafe.basic<#if DOMAIN != "">.${DOMAIN}</#if>.executor.query.${TYPE_NAME_UPPER_CAMEL}ByIdQryExe;
import vip.tuoyang.schoolsafe.basic<#if DOMAIN != "">.${DOMAIN}</#if>.executor.query.${TYPE_NAME_UPPER_CAMEL}PageQryExe;

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}ServiceImpl implements ${TYPE_NAME_UPPER_CAMEL}ServiceI {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}AddCmdExe ${TYPE_NAME_LOWER_CAMEL}AddCmdExe;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}PageQryExe ${TYPE_NAME_LOWER_CAMEL}PageQryExe;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}ByIdQryExe ${TYPE_NAME_LOWER_CAMEL}ByIdQryExe;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}UpdateCmdExe ${TYPE_NAME_LOWER_CAMEL}UpdateCmdExe;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}DelByIdCmdExe ${TYPE_NAME_LOWER_CAMEL}DelByIdCmdExe;

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void add${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        ${TYPE_NAME_LOWER_CAMEL}AddCmdExe.execute(cmd);
    }

    @Override
    public Page<${TYPE_NAME_UPPER_CAMEL}DTO> get${TYPE_NAME_UPPER_CAMEL}Page(${TYPE_NAME_UPPER_CAMEL}PageQry qry) {
        return ${TYPE_NAME_LOWER_CAMEL}PageQryExe.execute(qry);
    }

    @Override
    public ${TYPE_NAME_UPPER_CAMEL}DTO get${TYPE_NAME_UPPER_CAMEL}ById(Long id) {
        return ${TYPE_NAME_LOWER_CAMEL}ByIdQryExe.execute(id);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        ${TYPE_NAME_LOWER_CAMEL}UpdateCmdExe.execute(cmd);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void delete${TYPE_NAME_UPPER_CAMEL}ById(Long id) {
        ${TYPE_NAME_LOWER_CAMEL}DelByIdCmdExe.execute(id);
    }
}