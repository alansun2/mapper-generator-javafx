<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import com.xxxx.base.core.constants.BaseConstants;
import com.xxxx.base.core.util.AssertUtils;
<#assign suffixs = ["DO", "GatewayI", "DTO"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

import java.util.Optional;

/**
 * @author ${author}
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}ByIdQryExe {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}GatewayI ${TYPE_NAME_LOWER_CAMEL}GatewayI;

    public ${TYPE_NAME_UPPER_CAMEL}DTO execute(Long id) {
        final Optional<${TYPE_NAME_UPPER_CAMEL}DO> ${TYPE_NAME_LOWER_CAMEL}DoOpt = ${TYPE_NAME_LOWER_CAMEL}GatewayI.get${TYPE_NAME_UPPER_CAMEL}ById(id);
        AssertUtils.isTrue(${TYPE_NAME_LOWER_CAMEL}DoOpt.isPresent() && ${TYPE_NAME_LOWER_CAMEL}DoOpt.get().getIsDelete() == BaseConstants.IS_DELETE_0,
                "记录不存在", HttpStatus.NOT_FOUND.value());
        return this.convert(${TYPE_NAME_LOWER_CAMEL}DoOpt.get());
    }

    private ${TYPE_NAME_UPPER_CAMEL}DTO convert(${TYPE_NAME_UPPER_CAMEL}DO ${TYPE_NAME_LOWER_CAMEL}do) {
        <@ic.ignoreColumnCheck "${TYPE_NAME_UPPER_CAMEL}DTO" "${TYPE_NAME_LOWER_CAMEL}DTO" "${TYPE_NAME_LOWER_CAMEL}do" "DTO" "DO" FIELDS_UPPER_CAMELS IGNORE_FIELDS_MAP!/>
        return ${TYPE_NAME_LOWER_CAMEL}DTO;
    }
}