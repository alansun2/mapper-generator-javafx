<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import com.xxxx.base.core.bean.response.Page;
<#assign suffixs = ["Service", "DTO", "PageDTO", "Cmd", "PageQry"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

/**
 * @author ${author}
 * @date ${CUR_DATE_TIME}
 */
@RestController
@RequestMapping(value = "/${TYPE_NAME_LOWER_HYPHEN}")
public class ${TYPE_NAME_UPPER_CAMEL}Controller {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Service ${TYPE_NAME_LOWER_CAMEL}Service;

    /**
     * 添加${DOMAIN_DESC}
     *
     * @param cmd {@link ${TYPE_NAME_UPPER_CAMEL}Cmd}
     */
    @PostMapping
    public void add${TYPE_NAME_UPPER_CAMEL}(@Validated @RequestBody ${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        ${TYPE_NAME_LOWER_CAMEL}Service.add${TYPE_NAME_UPPER_CAMEL}(cmd);
    }

    /**
     * 分页获取${DOMAIN_DESC}列表
     *
     * @param qry {@link ${TYPE_NAME_UPPER_CAMEL}PageQry}
     * @return {@link ${TYPE_NAME_UPPER_CAMEL}PageDTO}s
     */
    @GetMapping(value = "/page")
    public Page<${TYPE_NAME_UPPER_CAMEL}PageDTO> get${TYPE_NAME_UPPER_CAMEL}Page(${TYPE_NAME_UPPER_CAMEL}PageQry qry) {
        return ${TYPE_NAME_LOWER_CAMEL}Service.get${TYPE_NAME_UPPER_CAMEL}Page(qry);
    }

    /**
     * 根据 id 获取${DOMAIN_DESC}
     *
     * @param id id
     * @return {@link ${TYPE_NAME_UPPER_CAMEL}DTO}
     */
    @GetMapping(value = "/{id}")
    public ${TYPE_NAME_UPPER_CAMEL}DTO get${TYPE_NAME_UPPER_CAMEL}ById(@PathVariable("id") Long id) {
        return ${TYPE_NAME_LOWER_CAMEL}Service.get${TYPE_NAME_UPPER_CAMEL}ById(id);
    }

    /**
     * 修改${DOMAIN_DESC}
     *
     * @param id  id
     * @param cmd {@link ${TYPE_NAME_UPPER_CAMEL}Cmd}
     */
    @PutMapping(value = "/{id}")
    public void update${TYPE_NAME_UPPER_CAMEL}ById(@PathVariable("id") Long id, @Validated @RequestBody ${TYPE_NAME_UPPER_CAMEL}Cmd cmd) {
        cmd.setId(id);
        ${TYPE_NAME_LOWER_CAMEL}Service.update${TYPE_NAME_UPPER_CAMEL}ById(cmd);
    }

    /**
     * 根据 id 删除${DOMAIN_DESC}
     *
     * @param id id
     */
    @DeleteMapping(value = "/{id}")
    public void delete${TYPE_NAME_UPPER_CAMEL}ById(@PathVariable("id") Long id) {
        ${TYPE_NAME_LOWER_CAMEL}Service.delete${TYPE_NAME_UPPER_CAMEL}ById(id);
    }
}