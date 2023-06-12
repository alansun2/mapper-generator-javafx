<#macro ignoreColumnCheck className name name1 suffixName1 suffixName2 items ignoreItemsMap>
        ${className} ${name} = new ${className}();
    <#assign ignoreItems1 = ignoreItemsMap[suffixName1]!>
    <#assign ignoreItems2 = ignoreItemsMap[suffixName2]!>
    <#list items as item>
        <#if ignoreItems1?seq_contains(item?lower_case) == false && ignoreItems2?seq_contains(item?lower_case) == false>
        ${name}.set${item}(${name1}.get${item}());
        </#if>
    </#list>
</#macro>

<#macro getPackage prefix suffixs packageMap>
    <#list suffixs as suffix>
        <#assign key = prefix + suffix>
        <#assign value = packageMap[key]!>
import ${value};
    </#list>
</#macro>