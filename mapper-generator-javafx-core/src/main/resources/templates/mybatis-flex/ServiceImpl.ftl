<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import ${FULL_SUPER_CLASS};
<#assign suffixs = ["", "Mapper", "Service"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>
import org.springframework.stereotype.Service;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}${CLASS_SUFFIX} extends ${SUPER_CLASS}<${TYPE_NAME_UPPER_CAMEL}Mapper, ${TYPE_NAME_UPPER_CAMEL}> implements ${TYPE_NAME_UPPER_CAMEL}Service {

}