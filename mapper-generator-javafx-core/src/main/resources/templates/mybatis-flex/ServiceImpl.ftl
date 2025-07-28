package ${PACKAGE};

import com.mybatisflex.spring.service.impl.ServiceImpl;
<#assign suffixs = ["", "Mapper", "Service"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>
import org.springframework.stereotype.Service;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}${CLASS_SUFFIX} extends ServiceImpl<${TYPE_NAME_UPPER_CAMEL}Mapper, ${TYPE_NAME_UPPER_CAMEL}> implements ${CUSTOM_PARAMS_MAP["CLASS_Service"]} {

}