<#import "ignoreCheck.ftl" as ic>
package ${PACKAGE};

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
<#assign suffixs = ["", "Mapper"]>
<@ic.getPackage suffixs CUSTOM_PARAMS_MAP/>

import com.sy.common.bo.Query;
import java.util.Optional;

/**
 * @author ${author}
 * @since ${CUR_DATE_TIME}
 */
@Repository
public class ${TYPE_NAME_UPPER_CAMEL}Dao {
    @Autowired
    private ${TYPE_NAME_UPPER_CAMEL}Mapper ${TYPE_NAME_LOWER_CAMEL}Mapper;

    @Override
    public void insertSelective(final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        ${TYPE_NAME_LOWER_CAMEL}Mapper.insertSelective(${TYPE_NAME_LOWER_CAMEL});
    }

    @Override
    public Page<${TYPE_NAME_UPPER_CAMEL}> queryList${TYPE_NAME_UPPER_CAMEL}(final Query query) {
        return ${TYPE_NAME_LOWER_CAMEL}Mapper.selectList(query);
    }

    @Override
    public Optional<${TYPE_NAME_UPPER_CAMEL}> getById(final Long id) {
        return Optional.ofNullable(${TYPE_NAME_LOWER_CAMEL}Mapper.selectByPrimaryKey(id));
    }

    @Override
    public int updateByPrimaryKeySelective(final ${TYPE_NAME_UPPER_CAMEL} ${TYPE_NAME_LOWER_CAMEL}) {
        return ${TYPE_NAME_LOWER_CAMEL}Mapper.updateByPrimaryKeySelective(${TYPE_NAME_LOWER_CAMEL});
    }

    @Override
    public int deleteByPrimaryKey(final Long id) {
        return ${TYPE_NAME_LOWER_CAMEL}Mapper.deleteByPrimaryKey(id);
    }
}