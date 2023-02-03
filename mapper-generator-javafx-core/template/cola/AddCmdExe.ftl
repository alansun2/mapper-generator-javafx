package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import vip.tuoyang.schoolsafe.${server}.domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DO;
import vip.tuoyang.schoolsafe.${server}.domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}GatewayI;
import vip.tuoyang.schoolsafe.${server}.dto<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}Cmd;

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}AddCmdExe {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}GatewayI ${TYPE_NAME_LOWER_CAMEL}GatewayI;

    public void execute(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        final ${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO = convert(cmd);
        ${TYPE_NAME_LOWER_CAMEL}GatewayI.save${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_LOWER_CAMEL}DO);
    }

    static ${TYPE_NAME_UPPER_CAMEL}DO convert(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        ${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO = new ${TYPE_NAME_UPPER_CAMEL}DO();
        <#list FIELDS_UPPER_CAMELS as item>
        ${TYPE_NAME_LOWER_CAMEL}DO.set${item}(cmd.get${item}());
        </#list>
        return ${TYPE_NAME_LOWER_CAMEL}DO;
    }
}