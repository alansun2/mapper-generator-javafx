<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
<#assign suffixs = ["DO", "GatewayI", "Cmd"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

/**
 * @author ${author}
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
        <@ic.ignoreColumnCheck "${TYPE_NAME_UPPER_CAMEL}DO" "${TYPE_NAME_LOWER_CAMEL}DO" "cmd" "DO" "Cmd" FIELDS_UPPER_CAMELS IGNORE_FIELDS_MAP!/>
        return ${TYPE_NAME_LOWER_CAMEL}DO;
    }
}