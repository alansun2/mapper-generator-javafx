package ${PACKAGE};

import vip.tuoyang.base.core.constants.BaseConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import vip.tuoyang.base.core.support.UserResourceHolder;
import <#if package_prefix != "">${package_prefix}.</#if>domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}DO;
import <#if package_prefix != "">${package_prefix}.</#if>domain<#if DOMAIN != "">.${DOMAIN}</#if>.${TYPE_NAME_UPPER_CAMEL}GatewayI;
import <#if package_prefix != "">${package_prefix}</#if><#if DOMAIN != "">.${DOMAIN}</#if>.mapper.${TYPE_NAME_UPPER_CAMEL}Mapper;

import java.util.Optional;

/**
 * @author ${author}
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}DelByIdCmdExe {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}GatewayI ${TYPE_NAME_LOWER_CAMEL}GatewayI;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Mapper ${TYPE_NAME_LOWER_CAMEL}Mapper;

    public void execute(Long id) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}DO> byIdOpt = ${TYPE_NAME_LOWER_CAMEL}GatewayI.get${TYPE_NAME_UPPER_CAMEL}ById(id);
        if (byIdOpt.isPresent() && byIdOpt.get().getIsDelete() == BaseConstants.IS_DELETE_0) {
            ${TYPE_NAME_LOWER_CAMEL}Mapper.deleteById(id, UserResourceHolder.getUserNameAndId());
        }
    }
}