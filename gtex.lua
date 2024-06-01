function includett(s, wrap, highlight)
   local high = ""
   if (highlight ~= nil) then
      high = "\\hisyntax{" .. highlight .. "}"
   end
   local file = io.open(s:gsub("%s", ""), "r")
   if (file == nil) then
      tex.print("\\opwarning{File " .. s .. " couldn't be included}")
      tex.print("??")
   else
      local g = file:read('*a')
      local s, _ = g:gsub('\\', '\\nbb')
      if wrap then
         tex.print('\\begtt ' .. high)
      end
      tex.print(s)
      if wrap then
         tex.print('\\endtt\n')
      end
   end
end

function insertAPImage(image, mul)
   local i = img.scan { filename = image };
   if (i == nil) then
      tex.print("??\\opwarning{File " .. image .. " could not be opened}")
      return
   end
   local ratio = math.min(tex.hsize / i.width, tex.vsize / i.height);
   i.height = i.height * ratio * mul;
   i.width = i.width * ratio * mul;
   img.write(i);
end

function guixref(id)
   local m = " guix:" .. id
   local mac = token.get_macro(m)
   if (mac == nil) then
      tex.print(m .. "??")
   else
      tex.print(mac)
   end
end

function load_guixrefs(path)
   local inputs_file = io.open(path, "r")
   if (inputs_file == nil) then
      tex.print("\\opwarning{The auxiliary file " .. path .. " does not exist yet, please run guix to create it}")
   else
      local s = inputs_file:read("*a")
      for k, v in s:gmatch("([^:]+):([^\n]*)\n") do
         token.set_macro(" guix:" .. k, v)
      end
   end
end

function escape_code(s)
   return s:gsub("\\", "\\\\"):gsub("\"", "\\\""):gsub("\n", "\\n")
end

gscriptf = io.open(tex.jobname .. ".scm", "w")
function guix(id, expr)
   gscriptf:write(id .. ":" .. expr .. "\n")
end

reading_code = false
reading_code_buffer = ""
reading_code_spec = ""
reading_code_idx = 0
function read_code(s)
   if (s == "") then
      reading_code_buffer = reading_code_buffer .. "\n"
      return s
   end
   first = utf8.codepoint(s)
   if reading_code then
      if (first == utf8.codepoint('»')) then
         reading_code = false
         ids = "code.src" .. reading_code_idx
         idr = "code.res" .. reading_code_idx
         src = "((@ (guix gexp) mixed-text-file) \"" .. ids ..
            "\" \"" .. escape_code(reading_code_buffer) .. "\" )"
         guix(ids, src)
         guix(idr,
              "((@ (gtex) eval-script) \"" .. reading_code_spec
              .. "\" " .. src .. ")")
         out = "\\vfil{\\everytt={\\typosize[24/27]}\\_includett{\\guixref{" .. ids .. "}}\\medskip\\hrule\\medskip\\_includett{\\guixref{" .. idr .. "}}}\\vfil"
         return out
      end
      if (reading_code_buffer == "") then
         reading_code_buffer = s
      else
         reading_code_buffer = reading_code_buffer .. "\n" .. s
      end
      return ""
   else
      if (first == utf8.codepoint('«')) then
         reading_code = true
         reading_code_buffer = ""
         reading_code_idx = reading_code_idx + 1
         reading_code_spec = s:sub(utf8.offset(s, 2))
         return ""
      end
      -- Don't touch, returning nil breaks LuaTeX or OpTeX somewhere
      return s
   end
end

callback.add_to_callback("process_input_buffer", read_code, "read_code")
