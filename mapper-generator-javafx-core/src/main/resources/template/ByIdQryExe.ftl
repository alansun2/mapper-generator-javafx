package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import vip.tuoyang.base.core.constants.BaseConstants;
import vip.tuoyang.base.core.util.AssertUtils;
import vip.tuoyang.schoolsafe.basic.domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DO;
import vip.tuoyang.schoolsafe.basic.domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}GatewayI;
import vip.tuoyang.schoolsafe.basic.dto.data.${TYPE_NAME_UPPER_CAMEL}DTO;

/**
 * @author AlanSun
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}ByIdQryExe {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}GatewayI ${TYPE_NAME_LOWER_CAMEL}GatewayI;

    public ${TYPE_NAME_UPPER_CAMEL}DTO execute(Long id) {
        final ${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO = ${TYPE_NAME_LOWER_CAMEL}GatewayI.get${DOMAIN_UPPER_CAMEL}ById(id);
        AssertUtils.isTrue(null != ${TYPE_NAME_LOWER_CAMEL}DO && ${TYPE_NAME_LOWER_CAMEL}DO.getIsDelete() == BaseConstants.IS_DELETE_0, "记录不存在", HttpStatus.NOT_FOUND.value());
        return this.convert(${TYPE_NAME_LOWER_CAMEL}DO);
    }

    private ${TYPE_NAME_UPPER_CAMEL}DTO convert(${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO) {
        ${TYPE_NAME_UPPER_CAMEL}DTO ${TYPE_NAME_LOWER_CAMEL}DTO = new ${TYPE_NAME_UPPER_CAMEL}DTO();
        <#list FIELDS_UPPER_CAMELS as item>
        ${TYPE_NAME_LOWER_CAMEL}DTO.set${item}(${TYPE_NAME_LOWER_CAMEL}DO.get${item}());
        </#list>
        return ${TYPE_NAME_LOWER_CAMEL}DTO;
    }
}