<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import vip.tuoyang.base.core.constants.BaseConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import vip.tuoyang.base.core.support.UserResourceHolder;
<#assign suffixs = ["DO", "GatewayI", "Mapper"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

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