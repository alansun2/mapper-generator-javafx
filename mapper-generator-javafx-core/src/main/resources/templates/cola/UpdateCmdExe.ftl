<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import com.xxxx.base.core.constants.BaseConstants;
import com.xxxx.base.core.util.AssertUtils;
<#assign suffixs = ["DO", "GatewayI", "Cmd"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

import java.util.Optional;

import static <#if package_prefix??>${package_prefix}</#if><#if DOMAIN != "">.${DOMAIN}</#if>.executor.${TYPE_NAME_UPPER_CAMEL}AddCmdExe.convert;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}UpdateCmdExe {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}GatewayI ${TYPE_NAME_LOWER_CAMEL}GatewayI;

    public void execute(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}DO> ${TYPE_NAME_LOWER_CAMEL}DoOpt = ${TYPE_NAME_LOWER_CAMEL}GatewayI.get${TYPE_NAME_UPPER_CAMEL}ById(cmd.getId());
        AssertUtils.isTrue(${TYPE_NAME_LOWER_CAMEL}DoOpt.isPresent() && ${TYPE_NAME_LOWER_CAMEL}DoOpt.get().getIsDelete() == BaseConstants.IS_DELETE_0,
                "记录不存在", HttpStatus.NOT_FOUND.value());
        final ${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}DO = convert(cmd);
        ${TYPE_NAME_LOWER_CAMEL}GatewayI.update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_LOWER_CAMEL}DO);
    }
}