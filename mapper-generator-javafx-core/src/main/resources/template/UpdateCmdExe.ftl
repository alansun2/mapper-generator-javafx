package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import vip.tuoyang.base.core.constants.BaseConstants;
import vip.tuoyang.base.core.util.AssertUtils;
import vip.tuoyang.schoolsafe.basic.domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DO;
import vip.tuoyang.schoolsafe.basic.domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}GatewayI;
import vip.tuoyang.schoolsafe.basic.dto.${TYPE_NAME_UPPER_CAMEL}Cmd;

import static vip.tuoyang.schoolsafe.basic<#if DOMAIN != "">.${DOMAIN}</#if>.executor.${TYPE_NAME_UPPER_CAMEL}AddCmdExe.convert;

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}UpdateCmdExe {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}GatewayI ${TYPE_NAME_LOWER_CAMEL}GatewayI;

    public void execute(${TYPE_NAME_UPPER_CAMEL}Cmd ${TYPE_NAME_LOWER_CAMEL}Cmd) {
        final ${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DOById = ${TYPE_NAME_LOWER_CAMEL}GatewayI.get${DOMAIN_UPPER_CAMEL}ById(${TYPE_NAME_LOWER_CAMEL}Cmd.getId());
        AssertUtils.isTrue(null != ${TYPE_NAME_LOWER_CAMEL}DOById && ${TYPE_NAME_LOWER_CAMEL}DOById.getIsDelete() == BaseConstants.IS_DELETE_0, "被操作记录不存在", HttpStatus.NOT_FOUND.value());
        final ${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO = convert(${TYPE_NAME_LOWER_CAMEL}Cmd);
        ${TYPE_NAME_LOWER_CAMEL}GatewayI.update${DOMAIN_UPPER_CAMEL}ById(${TYPE_NAME_LOWER_CAMEL}DO);
    }
}