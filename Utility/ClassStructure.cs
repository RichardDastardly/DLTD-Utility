using System;
using System.Collections.Generic;
using System.Reflection;

namespace DLTD.Utility
{
    public delegate bool AttributeMatch(Attribute attribute);

    public abstract class Entity
    {
        protected BindingFlags flags;
        public MemberInfo memberInfo;
        public string MemberName { get { return memberInfo?.Name; } }
        public Attribute[] Attributes {  get { return memberInfo?.GetCustomAttributes(true) as Attribute[]; } }
        protected Type HostType;
        protected object entity;
        public virtual Type EntityType
        {
            get { return typeof(object); }
        }

        public virtual void EstablishEntity() {}

        public Entity( Type Host, MemberInfo member, BindingFlags flags )
        {
            memberInfo = member;
            HostType = Host;
            this.flags = flags;
            EstablishEntity();
        }

        public Entity(Type Host, MemberInfo member) : this(Host, member, BindingFlags.Default) { }
        public Entity(object Host, MemberInfo member) : this(Host.GetType(), member) { }

        public T GetAttributeOfType<T>() where T : Attribute
        {
            if ( HasAttributeOfType<T>())
                return (T)Attribute.GetCustomAttribute(memberInfo, typeof(T));
            return null;
        }

        public bool HasAttributeOfType<T>() where T : Attribute
        {
            return Attribute.IsDefined(memberInfo, typeof(T));
        }

        public bool HasAttributeOfType<T>(AttributeMatch match) where T : Attribute
        {
            if (match != null && HasAttributeOfType<T>())
            {
                var matchedAttribs = Attribute.GetCustomAttributes(memberInfo, typeof(T), true);
                for (int i = 0; i < matchedAttribs.Length; i++)
                    if (match(matchedAttribs[i]))
                        return true;
            }
            return false;
        }

    }

    public class Member: Entity
    {
        public Member(Type Host, MemberInfo member) : base(Host, member) {}
        public Member(Type Host, MemberInfo member, BindingFlags flags) : base(Host, member, flags) { }

        public virtual object GetValue(object Host) { return null; }
        public virtual void SetValue<T>(object Host, T value) { }
    }

    public class MemberField : Member
    {
        public MemberField(Type Host, MemberInfo member, BindingFlags flags ) : base(Host, member, flags ) { }
        public MemberField(Type Host, MemberInfo member) : base(Host, member, BindingFlags.Default ) {}
        public override Type EntityType
        {
            get { return (memberInfo as FieldInfo).FieldType; }
        }

        public override void EstablishEntity() {
            entity = HostType.GetField(MemberName, flags );
            if (entity == null)
                throw new Exception(HostType.ToString()+ ".GetField(" + MemberName + "), " + flags.ToString() + " returned null: check flags");
        }

        public override object GetValue(object Host) {
            return (entity != null ) ? (entity as FieldInfo).GetValue(Host) : null;
        }

        public override void SetValue<T>(object Host, T value)  {
            (entity as FieldInfo).SetValue(Host, value);
        }
    }

    public class MemberProperty: Member
    {
        public MemberProperty(Type Host, MemberInfo member, BindingFlags flags ) : base(Host, member, flags ) {}
        public override Type EntityType
        {
            get
            {
                return (memberInfo as PropertyInfo).PropertyType;
            }
        }

        public override void EstablishEntity()
        {
            entity = HostType.GetProperty(MemberName, flags);
            if (entity == null)
                throw new Exception(HostType.ToString() + ".GetProperty(" + MemberName + "), " + flags.ToString() + " returned null: check flags");
        }

        public override object GetValue(object Host)
        {
            var e = entity as PropertyInfo;
            if ( e.CanRead )
                return (entity != null ) ? (entity as PropertyInfo).GetValue(Host, null) : null;
            return null;
        }

        public override void SetValue<T>(object Host, T value)
        {
            var e = entity as PropertyInfo;
            if( e.CanWrite )
                e.SetValue(Host, value, null);
        }
    }

    public class MemberMethod : Member
    {
        public MemberMethod(Type Host, MemberInfo member, BindingFlags flags ) : base(Host, member, flags ) {}

        public override void EstablishEntity()
        {
            entity = HostType.GetMethod(MemberName, flags);
            if (entity == null)
                throw new Exception(HostType.ToString() + ".GetMethod(" + MemberName + "), " + flags.ToString() + " returned null: check flags");
        }
    }


/// <summary>
/// Cache & basic accessor methods for class structure via reflection.
/// Should be used as a static class member
/// </summary>
    public class ClassStructure
    {
        private Type hostType;
        private BindingFlags flags;
        public Dictionary<string, Member> Members;
        public Dictionary<string, MemberField> Fields;
        public Dictionary<string, MemberProperty> Properties;
        public Dictionary<string, Member> FieldsAndProperties;
        public Dictionary<string, MemberMethod> Methods;

        public ClassStructure( Type host, BindingFlags flags  )
        {
            if (host == null)
                throw new Exception("ClassStructure given a null entity!");

            hostType = host;
            this.flags = flags;
            var hostMembers = hostType.GetMembers(flags);

            Members = new Dictionary<string, Member>();
            Fields = new Dictionary<string, MemberField>();
            Properties = new Dictionary<string, MemberProperty>();
            FieldsAndProperties = new Dictionary<string, Member>();
            Methods = new Dictionary<string, MemberMethod>();

            for( int i = 0; i < hostMembers.Length; i++ )
            {
                var memberName = hostMembers[i].Name;
                switch(hostMembers[i].MemberType)
                {
                    case MemberTypes.Field:
                        var newField = new MemberField(host, hostMembers[i], flags );
                        Members.Add(memberName, newField);
                        Fields.Add(memberName, newField);
                        FieldsAndProperties.Add(memberName, newField);
                        break;

                    case MemberTypes.Property:
                        var newProperty = new MemberProperty(host, hostMembers[i], flags);
                        Members.Add(memberName, newProperty);
                        Properties.Add(memberName, newProperty);
                        FieldsAndProperties.Add(memberName, newProperty);
                        break;

                    case MemberTypes.Method:
                        var newMethod = new MemberMethod(host, hostMembers[i], flags);
                        Members.Add(memberName, newMethod);
                        Methods.Add(memberName, newMethod);
                        break;
                }
            }
        }

        public ClassStructure(Type Host) : this(Host, BindingFlags.Default ) { }
        public ClassStructure( object hostInstance ) : this( hostInstance.GetType()) {}

        public Dictionary<string,Member> GetMembersWithAttribute<T>() where T : Attribute
        {
            var result = new Dictionary<string, Member>();
            foreach( var member in Members )
            {
                if (member.Value.HasAttributeOfType<T>())
                    result[member.Key] = member.Value;
            }

            return result;
        }

        public Dictionary<string, Member> GetMembersWithAttribute<T>( AttributeMatch match ) where T : Attribute
        {
            var result = new Dictionary<string, Member>();
            foreach (var member in Members)
            {
                if (member.Value.HasAttributeOfType<T>( match ))
                    result[member.Key] = member.Value;
            }

            return result;
        }

        public T GetHostAttributeOfType<T>() where T : Attribute
        {
            if (Attribute.IsDefined(hostType, typeof(T)))
                return Attribute.GetCustomAttribute(hostType, typeof(T)) as T;

            return null;
        }

        public T[] GetAllHostAttributesOfType<T>() where T : Attribute
        {
            if (Attribute.IsDefined(hostType, typeof(T)))
                return Attribute.GetCustomAttributes(hostType, typeof(T), true) as T[];

            return null;
        }

        // probably of limited use
        /// <summary>
        /// Attempts to retrieve all field & property values.
        /// </summary>
        /// <param name="Instance"></param>
        /// <returns></returns>
        public Dictionary<string,object> GetValues( object Instance )
        {
            // assume we passed the right type of obejct...
            var result = new Dictionary<string, object>();
            var _keys = new List<string>(FieldsAndProperties.Keys);

            for (var i = 0; i < _keys.Count; i++)
                result.Add(_keys[i], FieldsAndProperties[_keys[i]].GetValue(Instance));

            return result;
        }
    }
}
