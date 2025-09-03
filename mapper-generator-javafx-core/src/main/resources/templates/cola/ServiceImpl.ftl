<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.xxxx.base.core.bean.response.Page;
<#assign suffixs = ["ServiceI", "DTO", "PageDTO", "Cmd", "PageQry", "AddCmdExe", "DelByIdCmdExe", "UpdateCmdExe", "ByIdQryExe", "PageQryExe"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

/**
 * @author ${author}
 * @date ${CUR_DATE_TIME}
 */
@Service
public class ${TYPE_NAME_UPPER_CAMEL}ServiceImpl implements ${TYPE_NAME_UPPER_CAMEL}ServiceI {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}AddCmdExe ${TYPE_NAME_LOWER_CAMEL}AddCmdExe;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}PageQryExe ${TYPE_NAME_LOWER_CAMEL}PageQryExe;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}ByIdQryExe ${TYPE_NAME_LOWER_CAMEL}ByIdQryExe;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}UpdateCmdExe ${TYPE_NAME_LOWER_CAMEL}UpdateCmdExe;
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}DelByIdCmdExe ${TYPE_NAME_LOWER_CAMEL}DelByIdCmdExe;

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void add${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        ${TYPE_NAME_LOWER_CAMEL}AddCmdExe.execute(cmd);
    }

    @Override
    public Page<${TYPE_NAME_UPPER_CAMEL}PageDTO> page${TYPE_NAME_UPPER_CAMEL}(${TYPE_NAME_UPPER_CAMEL}PageQry qry) {
        return ${TYPE_NAME_LOWER_CAMEL}PageQryExe.execute(qry);
    }

    @Override
    public ${TYPE_NAME_UPPER_CAMEL}DTO get${TYPE_NAME_UPPER_CAMEL}ById(Long id) {
        return ${TYPE_NAME_LOWER_CAMEL}ByIdQryExe.execute(id);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void update${TYPE_NAME_UPPER_CAMEL}ById(${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        ${TYPE_NAME_LOWER_CAMEL}UpdateCmdExe.execute(cmd);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void delete${TYPE_NAME_UPPER_CAMEL}ById(Long id) {
        ${TYPE_NAME_LOWER_CAMEL}DelByIdCmdExe.execute(id);
    }
}